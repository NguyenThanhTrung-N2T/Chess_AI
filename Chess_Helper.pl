% Get the color of opponent
opponent_color(white, black).
opponent_color(black, white).

% --- Utility function ---
abs(X, Y) :- X >= 0, Y is X; X < 0, Y is -X.

between_min_max(A, B, Min, Max) :-
    (A < B -> Min is A+1, Max is B-1 ; Min is B+1, Max is A-1).

% --- Kiểm tra đường thẳng ---
clear_straight(C1, R1, C2, R2) :-
    (C1 =:= C2 ->
        between_min_max(R1, R2, MinR, MaxR),
        % Ensure R_intermediate is in the board, though between_min_max thường đảm bảo điều này nếu C1,R1,C2,R2 hợp lệ
        \+ (between(MinR, MaxR, R_intermediate), R_intermediate >= 1, R_intermediate =< 8, piece_at(C1, R_intermediate, _, _))
    ;
     R1 =:= R2 ->
        between_min_max(C1, C2, MinC, MaxC),
        % Đảm bảo C_intermediate nằm trong bàn cờ
        \+ (between(MinC, MaxC, C_intermediate), C_intermediate >= 1, C_intermediate =< 8, piece_at(C_intermediate, R1, _, _))
    ).

% --- Kiểm tra đường chéo ---
clear_diagonal(C1, R1, C2, R2) :-
    % abs(C1 - C2) =:= abs(R1 - R2), % Điều kiện này đã có ở is_attacking(bishop,...)
    ColStep is sign(C2 - C1),
    RowStep is sign(R2 - R1),
    NextC is C1 + ColStep,
    NextR is R1 + RowStep,
    clear_diagonal_step(NextC, NextR, C2, R2, ColStep, RowStep).


% Dừng khi vị trí hiện tại là ngay TRƯỚC ô đích
clear_diagonal_step(C, R, C2, R2, _, _) :-
    (C =:= C2, R =:= R2), !. % Tới đích thì dừng, không kiểm tra ô đích

clear_diagonal_step(C, R, C2, R2, ColStep, RowStep) :-
    % Ô trung gian (C,R) phải nằm trong bàn cờ
    C >= 1, C =< 8, R >= 1, R =< 8,
    \+ piece_at(C, R, _, _), % Nếu ô hiện tại không bị cản
    NextC is C + ColStep,
    NextR is R + RowStep,
    clear_diagonal_step(NextC, NextR, C2, R2, ColStep, RowStep).


% --- Bishop chỉ kiểm tra đường đi ---
bishop_move_path(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    C1 \= C2,
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_diagonal(C1, R1, C2, R2).

% --- Rook chỉ kiểm tra đường đi ---
rook_move_path(C1, R1, C2, R2) :-
    (C1 =:= C2; R1 =:= R2),
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_straight(C1, R1, C2, R2).


% --- Check that the piece is not capturing a piece of the same color. ---
not_same_color(C2, R2, Color) :-
    (\+ piece_at(C2, R2, _, _));
    (piece_at(C2, R2, OtherColor, _), OtherColor \= Color).

% --- Hàm phong cấp quân cờ ---
promote_pawn(C, R, Color, NewPiece) :-
    retract(piece_at(C, R, Color, pawn)),
    assertz(piece_at(C, R, Color, NewPiece)).


castle_king_side(Color, C1, R1, C2, R2) :-
    % Vị trí vua và xe cho nhập thành bên vua
    (Color = white -> ExpectedR = 1 ; Color = black -> ExpectedR = 8),
    C1 = 5, R1 = ExpectedR, % Vua ở vị trí e1/e8
    C2 = 7, R2 = ExpectedR, % Vua di chuyển đến g1/g8
    RookActualCol = 8, % Xe ở cột H
    % Vua và xe chưa di chuyển
    \+ king_moved(Color),
    \+ rook_moved(Color, RookActualCol), % Kiểm tra xe ở cột H (8) chưa di chuyển
    % Các ô giữa vua và xe phải trống
    % clear_straight giữa vua (C1,R1) và vị trí gốc của xe (RookActualCol, R1)
    clear_straight(C1, R1, RookActualCol, R1),
    % Vua không bị chiếu, không đi qua hoặc đứng ở ô bị chiếu
    \+ in_check(Color),
    KingPathCol1 is C1 + 1, % Ô f1/f8
    \+ causes_check(king, Color, C1, R1, KingPathCol1, R1), % Vua không đi qua ô bị chiếu (f1/f8)
    % KingPathCol2 is C1 + 2 (ô g1/g8, chính là C2,R2)
    \+ causes_check(king, Color, C1, R1, C2, R2). % Vua không đáp xuống ô bị chiếu (g1/g8)

castle_queen_side(Color, C1, R1, C2, R2) :-
    % Vị trí vua và xe cho nhập thành bên hậu
    (Color = white -> ExpectedR = 1 ; Color = black -> ExpectedR = 8),
    C1 = 5, R1 = ExpectedR, % Vua ở vị trí e1/e8
    C2 = 3, R2 = ExpectedR, % Vua di chuyển đến c1/c8
    RookActualCol = 1, % Xe ở cột A
    % Vua và xe chưa di chuyển
    \+ king_moved(Color),
    \+ rook_moved(Color, RookActualCol), % Kiểm tra xe ở cột A (1) chưa di chuyển
    % Các ô giữa vua và xe phải trống
    % clear_straight giữa vua (C1,R1) và vị trí gốc của xe (RookActualCol, R1)
    clear_straight(C1, R1, RookActualCol, R1),
    % Vua không bị chiếu, không đi qua hoặc đứng ở ô bị chiếu
    \+ in_check(Color),
    KingPathCol1 is C1 - 1, % Ô d1/d8
    \+ causes_check(king, Color, C1, R1, KingPathCol1, R1), % Vua không đi qua ô bị chiếu (d1/d8)
    % KingPathCol2 is C1 - 2 (ô c1/c8, chính là C2,R2)
    \+ causes_check(king, Color, C1, R1, C2, R2). % Vua không đáp xuống ô bị chiếu (c1/c8)

% --- Kiểm tra tấn công ---
is_attacking(pawn, white, C1, R1, C2, R2) :-
    R2 is R1 + 1,
    (C2 is C1 + 1 ; C2 is C1 - 1).

is_attacking(pawn, black, C1, R1, C2, R2) :-
    R2 is R1 - 1,
    (C2 is C1 + 1 ; C2 is C1 - 1).

is_attacking(knight, _, C1, R1, C2, R2) :-
    (abs(C1 - C2) =:= 2, abs(R1 - R2) =:= 1) ;
    (abs(C1 - C2) =:= 1, abs(R1 - R2) =:= 2).

is_attacking(bishop, _, C1, R1, C2, R2) :-
    (C1 \= C2 ; R1 \= R2), % Quân cờ không tấn công chính nó
    abs(C1 - C2) =:= abs(R1 - R2),
    clear_diagonal(C1, R1, C2, R2).

is_attacking(rook, _, C1, R1, C2, R2) :-
    (C1 \= C2 ; R1 \= R2), % Quân cờ không tấn công chính nó
    (C1 =:= C2 ; R1 =:= R2),
    clear_straight(C1, R1, C2, R2).

is_attacking(queen, _, C1, R1, C2, R2) :-
    (is_attacking(bishop, _, C1, R1, C2, R2) ;
    is_attacking(rook, _, C1, R1, C2, R2)).


is_attacking(king, _, C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1,
    abs(R1 - R2) =< 1,
    (C1 \= C2 ; R1 \= R2).


% Chụp nguyên bàn cờ hiện tại
snapshot(Snapshot) :-
    findall(piece_at(C, R, Color, Piece), piece_at(C, R, Color, Piece), SnapshotUnsorted),
    sort(SnapshotUnsorted, Snapshot).

% Push trạng thái hiện tại vào lịch sử
push_board_history :-
    snapshot(Current),
    retract(board_history(History)),
    assertz(board_history([Current | History])).

% Đếm số lần trạng thái hiện tại đã xuất hiện
count_occurrences(Current, Count) :-
    board_history(History),
    include(=(Current), History, Matches),
    length(Matches, Count).