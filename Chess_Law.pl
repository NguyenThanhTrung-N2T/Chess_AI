% Chess_Law.pl

% --- Khai báo dynamic ---
:- dynamic piece_at/4.
:- dynamic board_history/1.
:- dynamic moved/3. % moved(Color, Piece, Col) % Lưu trạng thái quân đã di chuyển
:- dynamic last_move/4. % Lưu nước đi cuối cùng để bắt tốt en passant
last_move(_,_,_,_).

% --- Hàm hỗ trợ ---
abs(X, Y) :- X >= 0, Y is X; X < 0, Y is -X.

% --- Pawn ---
pawn_dir(white, 1, 2).
pawn_dir(black, -1, 7).

% Di chuyển thẳng (không ăn) của quân tốt
pawn_move(Color, Col, Row, Col, Row2) :-
    pawn_dir(Color, Dir, StartRow),
    (
        (Row2 is Row + Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row2, _, _)) % đi thẳng 1 ô
        ;
        (Row =:= StartRow, Row2 is Row + 2*Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row + Dir, _, _),
            \+ piece_at(Col, Row2, _, _)) % đi 2 ô từ vị trí xuất phát
    ),
    Col >= 1, Col =< 8.

% Ăn chéo của quân tốt
pawn_capture(Color, Col1, Row1, Col2, Row2) :-
    pawn_dir(Color, Dir, _),
    Row2 is Row1 + Dir,
    (Col2 is Col1 + 1; Col2 is Col1 - 1),
    Col2 >= 1, Col2 =< 8,
    Row2 >= 1, Row2 =< 8,
    piece_at(Col2, Row2, OtherColor, _),
    Color \= OtherColor.

% --- Bắt tốt qua đường (en passant) ---
en_passant(Color, Col1, Row1, Col2, Row2) :-
    pawn_dir(Color, Dir, _),
    (Color = white -> Row1 =:= 5 ; Row1 =:= 4), % Trắng ở hàng 5, Đen ở hàng 4
    Row2 is Row1 + Dir,
    (Col2 is Col1 + 1; Col2 is Col1 - 1),
    piece_at(Col2, Row1, OtherColor, pawn),
    Color \= OtherColor,
    last_move(Col2, Row3, Col2, Row1),
    pawn_dir(OtherColor, OtherDir, _),
    Row3 =:= Row1 + 2*OtherDir,
    \+ piece_at(Col2, Row2, _, _).    

% --- Knight ---
knight_move(C1, R1, C2, R2) :-
    (
        (abs(C1 - C2) =:= 2, abs(R1 - R2) =:= 1);
        (abs(C1 - C2) =:= 1, abs(R1 - R2) =:= 2)
    ),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, knight), Color1 \= Color2)).

% --- Bishop ---
bishop_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    C1 \= C2,
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_diagonal(C1, R1, C2, R2),
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, bishop), Color1 \= Color2)).

% --- Rook ---
rook_move(C1, R1, C2, R2) :-
    (C1 =:= C2; R1 =:= R2),
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_straight(C1, R1, C2, R2),
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, rook), Color1 \= Color2)).

% --- Queen ---
queen_move(C1, R1, C2, R2) :-
    (bishop_move(C1, R1, C2, R2);
     rook_move(C1, R1, C2, R2)).

% --- King ---
king_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1, abs(R1 - R2) =< 1,
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, king), Color1 \= Color2)).

% --- Hỗ trợ kiểm tra đường đi ---
clear_straight(C1, R1, C2, R2) :-
    (C1 =:= C2 ->
        between_min_max(R1, R2, Min, Max),
        \+ (between(Min, Max, R), R \= R1, R \= R2, piece_at(C1, R, _, _))
    ;
     R1 =:= R2 ->
        between_min_max(C1, C2, Min, Max),
        \+ (between(Min, Max, C), C \= C1, C \= C2, piece_at(C, R1, _, _))
    ).
% --- Hỗ trợ kiểm tra đường chéo ---
clear_diagonal(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    ColStep is (C2 - C1) // abs(C2 - C1),
    RowStep is (R2 - R1) // abs(R2 - R1),
    clear_diagonal_step(C1, R1, C2, R2, ColStep, RowStep).
%% Hàm đệ quy kiểm tra từng bước trên đường chéo
clear_diagonal_step(C, R, C2, R2, _, _) :-
    C1 is C + ((C2 - C) // abs(C2 - C)),
    R1 is R + ((R2 - R) // abs(R2 - R)),
    C1 =:= C2, R1 =:= R2, !.
clear_diagonal_step(C, R, C2, R2, ColStep, RowStep) :-
    C1 is C + ColStep,
    R1 is R + RowStep,
    (C1 =:= C2, R1 =:= R2 -> true
    ;
        (\+ piece_at(C1, R1, _, _)),
        clear_diagonal_step(C1, R1, C2, R2, ColStep, RowStep)
    ).
% --- Hàm hỗ trợ kiểm tra khoảng giữa ---
between_min_max(A, B, Min, Max) :-
    (A < B -> Min is A+1, Max is B-1 ; Min is B+1, Max is A-1).

% --- Luật tổng quát kiểm tra nước đi hợp lệ ---
legal_move(pawn, Color, C1, R1, C2, R2) :-
    (pawn_move(Color, C1, R1, C2, R2) ;
     pawn_capture(Color, C1, R1, C2, R2) ;
     en_passant(Color, C1, R1, C2, R2)),
    not_same_color(C2, R2, Color).
legal_move(knight, Color, C1, R1, C2, R2) :-
    knight_move(C1, R1, C2, R2),
    not_same_color(C2, R2, Color).
legal_move(bishop, Color, C1, R1, C2, R2) :-
    bishop_move(C1, R1, C2, R2),
    not_same_color(C2, R2, Color).
legal_move(rook, Color, C1, R1, C2, R2) :-
    rook_move(C1, R1, C2, R2),
    not_same_color(C2, R2, Color).
legal_move(queen, Color, C1, R1, C2, R2) :-
    queen_move(C1, R1, C2, R2),
    not_same_color(C2, R2, Color).
legal_move(king, Color, 5, Row, 7, Row) :- castle_kingside(Color).
legal_move(king, Color, 5, Row, 3, Row) :- castle_queenside(Color).
legal_move(king, Color, C1, R1, C2, R2) :-
    king_move(C1, R1, C2, R2),
    not_same_color(C2, R2, Color).

not_same_color(C2, R2, Color) :-
    (\+ piece_at(C2, R2, _, _));
    (piece_at(C2, R2, OtherColor, _), OtherColor \= Color).

% --- Lưu & kiểm tra lịch sử bàn cờ ---
board_history([]).
% --- Lưu trạng thái bàn cờ ---
save_board_state :-
    findall(piece_at(C, R, Color, Piece), piece_at(C, R, Color, Piece), CurrentState),
    retract(board_history(History)),
    NewHistory = [CurrentState | History],
    assertz(board_history(NewHistory)).
% --- Xóa lịch sử bàn cờ ---
clear_board_history :-
    retractall(board_history(_)),
    assertz(board_history([])).

% --- Undo ---
undo_move :-
    board_history([_Current | RestHistory]),
    retractall(piece_at(_, _, _, _)),
    ( RestHistory = [PreviousState | _] ->
        forall(member(piece_at(C, R, Color, Piece), PreviousState),
               assertz(piece_at(C, R, Color, Piece)))
    ; true ),
    retract(board_history(_)),
    assertz(board_history(RestHistory)).

% --- Hàm di chuyển quân có lưu trạng thái ---
move(Color, C1, R1, C2, R2) :-
    piece_at(C1, R1, Color, Piece),
    (piece_at(C2, R2, OtherColor, _) -> WasCapture = true ; WasCapture = false),
    legal_move_safe(Piece, Color, C1, R1, C2, R2),
    save_board_state,
    retract(piece_at(C1, R1, Color, Piece)),
    (   Piece = pawn,
        en_passant(Color, C1, R1, C2, R2)
    ->  pawn_dir(Color, Dir, _),
        RowPawn is R2 - Dir,
        retract(piece_at(C2, RowPawn, _, pawn))
    ;   true
    ),
    (retract(piece_at(C2, R2, _, _)); true),
    assertz(piece_at(C2, R2, Color, Piece)),
    retractall(last_move(_,_,_,_)),
    assertz(last_move(C1, R1, C2, R2)),
    update_fifty_move_counter(Piece, WasCapture),
    (Piece = king -> assertz(moved(Color, king, C1)) ; true),
    (Piece = rook, (C1 =:= 1 ; C1 =:= 8) -> assertz(moved(Color, rook, C1)) ; true).

% Tìm vị trí vua của màu Color
king_position(Color, Col, Row) :-
    piece_at(Col, Row, Color, king).

% Có quân đối phương nào tấn công vua không?
attacked(Color, Col, Row) :-
    Opponent = (Color = white -> black ; white),
    piece_at(C, R, Opponent, Piece),
    legal_move(Piece, Opponent, C, R, Col, Row).

% Kiểm tra vua bị chiếu
in_check(Color) :-
    king_position(Color, Col, Row),
    attacked(Color, Col, Row).

% Nước đi hợp lệ thực sự: không làm vua bị chiếu
legal_move_safe(Piece, Color, C1, R1, C2, R2) :-
    legal_move(Piece, Color, C1, R1, C2, R2),
    save_board_state,
    retract(piece_at(C1, R1, Color, Piece)),
    (retract(piece_at(C2, R2, _, _)); true),
    assertz(piece_at(C2, R2, Color, Piece)),
    \+ in_check(Color),  % Vua không bị chiếu sau nước đi
    undo_move.

% Có nước đi hợp lệ nào cho Color không?
has_legal_move(Color) :-
    piece_at(C1, R1, Color, Piece),
    between(1,8,C2), between(1,8,R2),
    legal_move_safe(Piece, Color, C1, R1, C2, R2). 

% Chiếu hết: vua bị chiếu và không còn nước đi hợp lệ nào
checkmate(Color) :-
    in_check(Color),
    \+ has_legal_move(Color).

% Kiểm tra nhập thành (castling) cho bên vua
castle_kingside(Color) :-
    (Color = white -> Row = 1 ; Row = 8),
    \+ moved(Color, king, 5),
    \+ moved(Color, rook, 8),
    \+ piece_at(6, Row, _, _),
    \+ piece_at(7, Row, _, _),
    \+ in_check(Color),
    \+ attacked(Color, 6, Row),
    \+ attacked(Color, 7, Row).

% Kiểm tra nhập thành (castling) cho bên hậu
castle_queenside(Color) :-
    (Color = white -> Row = 1 ; Row = 8),
    \+ moved(Color, king, 5),
    \+ moved(Color, rook, 1),
    \+ piece_at(2, Row, _, _),
    \+ piece_at(3, Row, _, _),
    \+ piece_at(4, Row, _, _),
    \+ in_check(Color),
    \+ attacked(Color, 4, Row),
    \+ attacked(Color, 3, Row).

% --- Thực hiện nhập thành kingside ---
move(Color, 5, Row, 7, Row) :-
    piece_at(5, Row, Color, king),
    legal_move_safe(king, Color, 5, Row, 7, Row),
    save_board_state,
    retract(piece_at(5, Row, Color, king)),
    retract(piece_at(8, Row, Color, rook)),
    assertz(piece_at(7, Row, Color, king)),
    assertz(piece_at(6, Row, Color, rook)),
    retractall(last_move(_,_,_,_)),
    assertz(last_move(5, Row, 7, Row)),
    assertz(moved(Color, king, 5)),
    assertz(moved(Color, rook, 8)).

% --- Thực hiện nhập thành queenside ---
move(Color, 5, Row, 3, Row) :-
    piece_at(5, Row, Color, king),
    legal_move_safe(king, Color, 5, Row, 3, Row),
    save_board_state,
    retract(piece_at(5, Row, Color, king)),
    retract(piece_at(1, Row, Color, rook)),
    assertz(piece_at(3, Row, Color, king)),
    assertz(piece_at(4, Row, Color, rook)),
    retractall(last_move(_,_,_,_)),
    assertz(last_move(5, Row, 3, Row)),
    assertz(moved(Color, king, 5)),
    assertz(moved(Color, rook, 1)).

% --- Luật kiểm tra nước đi phong cấp ---
legal_move(pawn, Color, C1, R1, C2, R2, promote(What)) :-
    (pawn_move(Color, C1, R1, C2, R2) ;
     pawn_capture(Color, C1, R1, C2, R2)),
    not_same_color(C2, R2, Color),
    ( (Color = white, R2 =:= 8) ; (Color = black, R2 =:= 1) ),
    member(What, [queen, rook, bishop, knight]).

% --- Thực hiện phong cấp tốt ---
move(Color, C1, R1, C2, R2, promote(What)) :-
    piece_at(C1, R1, Color, pawn),
    legal_move(pawn, Color, C1, R1, C2, R2, promote(What)),
    save_board_state,
    retract(piece_at(C1, R1, Color, pawn)),
    (retract(piece_at(C2, R2, _, _)); true),
    assertz(piece_at(C2, R2, Color, What)),
    retractall(last_move(_,_,_,_)),
    assertz(last_move(C1, R1, C2, R2)).

% Hòa cờ (stalemate): không có nước đi hợp lệ nào cho Color và vua không bị chiếu
stalemate(Color) :-
    \+ in_check(Color),
    \+ has_legal_move(Color).

% --- Kiểm tra ba lần lặp lại trạng thái bàn cờ ---
same_board_state(State1, State2) :- State1 == State2.
% --- Đếm số lần lặp lại trạng thái bàn cờ ---
count_repeated_states(Count) :-
    board_history(History),
    History = [Latest | Rest],
    include(same_board_state(Latest), Rest, Matches),
    length(Matches, Count).
% --- Kiểm tra ba lần lặp lại trạng thái bàn cờ ---
threefold_repetition :- count_repeated_states(Count), Count >= 3.


% --- Hòa do không đủ lực chiếu hết ---
insufficient_material :-
    findall(Type, (piece_at(_, _, _, Type), Type \= king), Pieces),
    sort(Pieces, Sorted),
    (
        Sorted = []; % chỉ còn hai vua
        Sorted = [bishop]; % vua với vua + 1 tượng
        Sorted = [knight]; % vua với vua + 1 mã
        (Sorted = [bishop, bishop], same_bishop_color) % vua với vua + 2 tượng cùng màu
    ).

% Kiểm tra hai tượng cùng màu ô
same_bishop_color :-
    findall((C, R), piece_at(C, R, _, bishop), Bishops),
    maplist(bishop_color, Bishops, Colors),
    list_to_set(Colors, Set),
    length(Set, 1).

% Xác định màu ô của tượng (đen/trắng)
bishop_color((C, R), black) :- 0 is (C + R) mod 2.
bishop_color((C, R), white) :- 1 is (C + R) mod 2.

% Hòa cờ 50 nước không ăn quân hay di chuyển quân
:- dynamic fifty_move_counter/1.
fifty_move_counter(0).

% Gọi hàm này trong move/5 và move/6, ngay sau khi thực hiện nước đi
update_fifty_move_counter(Piece, WasCapture) :-
    (Piece = pawn ; WasCapture = true) ->
        retractall(fifty_move_counter(_)), assertz(fifty_move_counter(0))
    ;
        (fifty_move_counter(N) -> true ; N = 0),
        N1 is N + 1,
        retractall(fifty_move_counter(_)), assertz(fifty_move_counter(N1)).

% Kiểm tra luật 50 nước
fifty_move_rule :- fifty_move_counter(N), N >= 100.
% Reset counter khi bắt đầu ván cờ mới 
reset_fifty_move_counter :-
    retractall(fifty_move_counter(_)),
    assertz(fifty_move_counter(0)).
