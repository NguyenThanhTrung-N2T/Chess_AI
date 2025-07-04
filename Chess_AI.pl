:- consult('Chess_Helper.pl').
:- consult('Chess_Law.pl'). % AI cần các định nghĩa luật từ Chess_Law

% --- Tính heuristic đơn giản dựa vào giá trị quân cờ ---
piece_value(pawn, 1).
piece_value(knight, 3).
piece_value(bishop, 3).
piece_value(rook, 5).
piece_value(queen, 9).
piece_value(king, 1000).  % King is the most valuable piece

% Đếm số quân bị tấn công (có thể bị ăn ở lượt sau)
count_attacked_pieces(Color, Count) :-
    findall((C, R, Piece),
        (piece_at(C, R, Color, Piece),
         opponent_color(Color, OppColor),
         piece_at(C2, R2, OppColor, OppPiece),
         legal_move(OppPiece, OppColor, C2, R2, C, R)), % Nếu đối thủ có thể ăn quân này
        AttackedList),
    length(AttackedList, Count).

% đánh giá bàn cờ dựa trên giá trị quân cờ và số quân bị tấn công
evaluate_board(Color, Score) :-
    findall(Value, (piece_at(_, _, Color, Piece), piece_value(Piece, Value)), MyValues),
    opponent_color(Color, OppColor),
    findall(Value, (piece_at(_, _, OppColor, Piece), piece_value(Piece, Value)), OppValues),
    sum_list(MyValues, MyScore),
    sum_list(OppValues, OppScore),
    count_attacked_pieces(Color, MyAttacked),
    count_attacked_pieces(OppColor, OppAttacked),
    % Thưởng lớn nếu chiếu bí, thưởng vừa phải nếu chỉ chiếu
    (in_check(OppColor), all_legal_moves(OppColor, []) -> CheckmateBonus = 10000 ; CheckmateBonus = 0),
    (in_check(OppColor), all_legal_moves(OppColor, Moves), Moves \= [] -> CheckBonus = 10 ; CheckBonus = 0),
    (in_check(Color) -> CheckMalus = -20 ; CheckMalus = 0),
    Score is MyScore - OppScore - MyAttacked + OppAttacked + CheckmateBonus + CheckBonus + CheckMalus.

% --- Tìm tất cả nước đi hợp lệ ---
all_legal_moves(Color, Moves) :-
    findall(Move,
            (   piece_at(C1, R1, Color, Piece),
                % Lặp qua tất cả các ô đích có thể
                between(1, 8, C2),
                between(1, 8, R2),
                (C1 \= C2 ; R1 \= R2), % Has to be a move, not standstill
                legal_move(Piece, Color, C1, R1, C2, R2), % Kiểm tra luật di chuyển cơ bản (từ Chess_Law.pl)
                \+ causes_check(Piece, Color, C1, R1, C2, R2), % Quan trọng: Nước đi không được tự làm vua bị chiếu
                Move = (Piece, C1, R1, C2, R2)
            ),
            RawMoves),
    sort(RawMoves, Moves). % sort để loại bỏ trùng lặp (nếu có) và chuẩn hóa

% --- Di chuyển giả lập ---
simulate_move(Piece, Color, C1, R1, C2, R2, Captured) :-
    (piece_at(C2, R2, OppColor, CapturedPiece) ->
        Captured = (C2, R2, OppColor, CapturedPiece),
        retract(piece_at(C2, R2, OppColor, CapturedPiece))
    ;
        Captured = none
    ),
    retract(piece_at(C1, R1, Color, Piece)),
    assertz(piece_at(C2, R2, Color, Piece)).

% --- Hoàn tác giả lập ---
undo_move(Piece, Color, C1, R1, C2, R2, none) :-
    retract(piece_at(C2, R2, Color, Piece)),
    assertz(piece_at(C1, R1, Color, Piece)).

undo_move(Piece, Color, C1, R1, C2, R2, (C2, R2, OppColor, CapturedPiece)) :-
    retract(piece_at(C2, R2, Color, Piece)),
    assertz(piece_at(C2, R2, OppColor, CapturedPiece)),
    assertz(piece_at(C1, R1, Color, Piece)).

% --- Minimax với Alpha-Beta Pruning (biến thể NegaMax) ---
% minimax(+Color, +Depth, +Alpha, +Beta, -Score, -Move)
% Score: Điểm đánh giá từ góc nhìn của 'Color'.
% Move: Nước đi tốt nhất tìm được cho 'Color' (hoặc 'none' nếu không có nước đi hợp lệ).
% Lấy giá trị quân bị ăn (nếu có)
capture_value((_, _, _, C2, R2), Value) :-
    piece_at(C2, R2, _, Piece), !,
    piece_value(Piece, Value).
capture_value(_, 0).

% Kiểm tra nước đi có chiếu đối thủ không
check_move((Piece, C1, R1, C2, R2), PlayerColor) :-
    simulate_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),
    opponent_color(PlayerColor, OppColor),
    (in_check(OppColor) -> Result = true ; Result = false),
    undo_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),
    Result == true.

% Tính độ ưu tiên cho nước đi: chiếu > ăn quân giá trị cao > thường
move_priority(Move, PlayerColor, Priority) :-
    (check_move(Move, PlayerColor) -> Priority1 = 100 ; Priority1 = 0),
    capture_value(Move, CaptureVal),
    Priority is Priority1 + CaptureVal.

% Sắp xếp nước đi theo độ ưu tiên
order_moves(Moves, PlayerColor, OrderedMoves) :-
    map_list_to_pairs({PlayerColor}/[Move, P]>>move_priority(Move, PlayerColor, P), Moves, Pairs),
    keysort(Pairs, SortedPairsRev),
    reverse(SortedPairsRev, SortedPairs),
    pairs_values(SortedPairs, OrderedMoves).

% Trường hợp cơ sở: Độ sâu bằng 0, đánh giá bàn cờ.
minimax(Color, 0, _Alpha, _Beta, Score, none) :- !, % Chú ý: BestMove ở đây là none vì đây là nút lá
    evaluate_board(Color, Score). % Đánh giá từ góc nhìn của người chơi hiện tại (Color)

% Trường hợp đệ quy: Độ sâu > 0
minimax(Color, Depth, Alpha, Beta, BestScore, BestMove) :-
    Depth > 0,
    % Tìm tất cả các nước đi thực sự hợp lệ cho người chơi hiện tại
    all_legal_moves(Color, Moves),
    order_moves(Moves, Color, OrderedMoves),
    (Moves == [] ->
        % Không có nước đi hợp lệ cho Color
        ( in_check(Color) -> % Kiểm tra xem có phải chiếu bí không (sử dụng in_check từ Chess_Law)
            % Chiếu bí: Điểm rất thấp cho người chơi hiện tại.
            % Cộng thêm Depth vào điểm để ưu tiên chiếu bí ở độ sâu lớn hơn một chút (delay).
            BestScore is -10000 - Depth, % Sử dụng một số âm lớn
            BestMove = none % Không có nước đi nào
        ; % Đó là hết nước đi (stalemate)
            % Hết nước đi: Điểm là 0 (hòa)
            BestScore is 0,
            BestMove = none % Không có nước đi nào
        )
    ; % Có các nước đi hợp lệ
        % Khởi tạo điểm tốt nhất với giá trị rất thấp, và nước đi tốt nhất là none
        InitialBestScore is -20001, % Thấp hơn bất kỳ điểm vật chất + điểm chiếu bí nào có thể có
        InitialBestMove = none,
        
        % Xử lý các nước đi với cắt tỉa Alpha-Beta
        opponent_color(Color, OppColor), % SỬA: Sử dụng opponent_color từ Chess_Helper.pl
        NextDepth is Depth - 1,
        
        % Gọi vị từ trợ giúp để lặp qua các nước đi và tìm nước tốt nhất
        process_moves(OrderedMoves, Color, OppColor, NextDepth, Alpha, Beta, InitialBestScore, InitialBestMove, BestScore, BestMove)
    ).

% process_moves(+MovesList, +PlayerColor, +OpponentColor, +CurrentDepth, +Alpha, +Beta, +CurrentBestScoreSoFar, +CurrentBestMoveSoFar, -FinalBestScore, -FinalBestMove)
% Vị từ trợ giúp cho minimax để lặp qua các nước đi với Alpha-Beta.

% Trường hợp cơ sở: Không còn nước đi nào để xử lý
process_moves([], _PlayerColor, _OpponentColor, _CurrentDepth, _Alpha, _Beta, CurrentBestScoreSoFar, CurrentBestMoveSoFar, CurrentBestScoreSoFar, CurrentBestMoveSoFar).

% Trường hợp đệ quy: Xử lý phần đầu của danh sách nước đi
process_moves([(Piece, C1, R1, C2, R2) | RestMoves], PlayerColor, OpponentColor, CurrentDepth, Alpha, Beta, CurrentBestScoreSoFar, CurrentBestMoveSoFar, FinalBestScore, FinalBestMove) :-
    CurrentMove = (Piece, C1, R1, C2, R2),
    
    % Mô phỏng nước đi hiện tại
    simulate_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),
    
    % Gọi đệ quy cho đối thủ. Alpha và Beta được đảo ngược và đổi dấu.
    NewAlphaForOpponent is -Beta,
    NewBetaForOpponent is -Alpha,
    minimax(OpponentColor, CurrentDepth, NewAlphaForOpponent, NewBetaForOpponent, OpponentRawScore, _OpponentMove), % _OpponentMove không dùng ở đây
    
    % Undo the move
    undo_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),

    % Tính điểm cho nước đi này từ góc nhìn của PlayerColor (NegaMax)
    ValueForThisMove is -OpponentRawScore,

    % Cập nhật điểm và nước đi tốt nhất tìm được cho đến nay
    ( ValueForThisMove > CurrentBestScoreSoFar ->
        UpdatedBestScore = ValueForThisMove,
        UpdatedBestMove = CurrentMove,
        UpdatedAlpha = max(Alpha, UpdatedBestScore) % Cập nhật Alpha
    ;
        UpdatedBestScore = CurrentBestScoreSoFar,
        UpdatedBestMove = CurrentBestMoveSoFar,
        UpdatedAlpha = Alpha % Alpha giữ nguyên
    ),

    % Kiểm tra cắt tỉa Beta
    ( UpdatedBestScore >= Beta ->
        % Cắt tỉa Beta: Nước đi này quá tốt cho người chơi hiện tại,
        % đối thủ sẽ ngăn chặn nhánh này xảy ra.
        % Chúng ta có thể cắt tỉa phần còn lại của các nước đi ở nút này.
        FinalBestScore = UpdatedBestScore, % Trả về điểm tìm được cho đến nay
        FinalBestMove = UpdatedBestMove
        % Đệ quy dừng ở đây, RestMoves không được xử lý.
    ;
        % Không cắt tỉa, tiếp tục xử lý các nước đi còn lại
        process_moves(RestMoves, PlayerColor, OpponentColor, CurrentDepth, UpdatedAlpha, Beta, UpdatedBestScore, UpdatedBestMove, FinalBestScore, FinalBestMove)
    ).

% Lần gọi ban đầu cho minimax từ bên ngoài
% minimax(+Color, +Depth, -Score, -Move)
minimax(Color, Depth, Score, Move) :-
    % Alpha ban đầu là âm vô cùng, Beta ban đầu là dương vô cùng
    minimax(Color, Depth, -20001, 20001, Score, Move). % Sử dụng các giá trị ngoài phạm vi điểm có thể có

% --- Nước đi ngẫu nhiên ---
% random_move(+Color, -Piece, -C1, -R1, -C2, -R2)
% Succeed if find a random legal move , Fail if not.
random_move(Color, Piece, C1, R1, C2, R2) :-
    all_legal_moves(Color, Moves),
    ( Moves == [] ->
        fail % Không có nước đi hợp lệ nào để chọn
    ;
        random_member((Piece, C1, R1, C2, R2), Moves)
    ).

% --- Tìm nước đi tốt nhất theo cấp độ ---
% find_best_move(+Color, +Level, -Piece, -C1, -R1, -C2, -R2)
% Succeed if find a legal move , Fail if not.
find_best_move(Color, Level, FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2) :-
    % Đầu tiên, kiểm tra xem có BẤT KỲ nước đi hợp lệ nào cho màu hiện tại không.
    % Nếu không, trò chơi kết thúc (chiếu bí hoặc hết nước đi), và AI nên thất bại.
    all_legal_moves(Color, Moves),
    ( Moves == [] ->
        fail % Không có nước đi hợp lệ, AI không thể di chuyển, báo hiệu thất bại cho Python
    ; % Có các nước đi hợp lệ, tiếp tục dựa trên cấp độ
        ( Level = easy ->
            % Cấp độ dễ: Chọn một nước đi hợp lệ ngẫu nhiên
            % random_move đã kiểm tra Moves \= [] bên trong nó, nhưng ở đây chúng ta đã biết Moves không rỗng.
            random_member((FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2), Moves)
        ; % Cấp độ trung bình hoặc khó: Sử dụng Minimax
            (Level = medium -> Depth = 2 ; Depth = 4),
            % Gọi thuật toán Minimax (phiên bản có 4 tham số)
            minimax(Color, Depth, _Score, BestMoveFound), % _Score không dùng ở đây
            % Minimax nên trả về một tuple nước đi hoặc 'none'
            ( BestMoveFound == none ->
                % Trường hợp này lý tưởng không nên xảy ra nếu all_legal_moves không rỗng,
                % nhưng để đề phòng, thất bại nếu minimax trả về none.
                % Điều này có thể xảy ra nếu có lỗi logic trong minimax khi xử lý nút gốc.
                fail
            ; % Minimax tìm thấy một nước đi
                BestMoveFound = (FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2) % Hợp nhất nước đi tìm được
            )
        )
    ).
