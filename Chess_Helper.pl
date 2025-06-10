% --- Kiểm tra đã di chuyển ---
:- dynamic has_moved/2.
:- dynamic has_moved/4.

% --- Hàm tiện ích ---
abs(X, Y) :- X >= 0, Y is X; X < 0, Y is -X.

between_min_max(A, B, Min, Max) :-
    (A < B -> Min is A+1, Max is B-1 ; Min is B+1, Max is A-1).

% --- Kiểm tra đường thẳng ---
clear_straight(C1, R1, C2, R2) :-
    (C1 =:= C2 ->
        between_min_max(R1, R2, Min, Max),
        \+ (between(Min, Max, R), R \= R1, R \= R2, piece_at(C1, R, _, _))
    ;
     R1 =:= R2 ->
        between_min_max(C1, C2, Min, Max),
        \+ (between(Min, Max, C), C \= C1, C \= C2, piece_at(C, R1, _, _))
    ).

% --- Kiểm tra đường chéo ---
clear_diagonal(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    ColStep is (C2 - C1) // abs(C2 - C1),
    RowStep is (R2 - R1) // abs(R2 - R1),
    clear_diagonal_step(C1, R1, C2, R2, ColStep, RowStep).

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

% --- Kiểm tra không ăn cùng màu ---
not_same_color(C2, R2, Color) :-
    (\+ piece_at(C2, R2, _, _));
    (piece_at(C2, R2, OtherColor, _), OtherColor \= Color).

% --- Kiểm tra vị trí vua ---
king_position(Color, Col, Row) :-
    piece_at(Col, Row, Color, king).

% --- Kiểm tra bị tấn công ---
attacked(Color, Col, Row) :-
    (Color = white -> Opponent = black ; Opponent = white),
    piece_at(C, R, Opponent, Piece),
    legal_move(Piece, Opponent, C, R, Col, Row).

% --- Kiểm tra chiếu ---
in_check(Color) :-
    king_position(Color, Col, Row),
    attacked(Color, Col, Row).


% --- Kiểm tra nhập thành kingside ---
castle_kingside(white) :-
    piece_at(5,1,white,king),
    piece_at(8,1,white,rook),
    clear_straight(5,1,8,1),
    \+ has_moved(white,king),
    \+ has_moved(white,rook,8,1),
    \+ in_check(white),
    \+ attacked(black,5,1),
    \+ attacked(black,6,1),
    \+ attacked(black,7,1).

castle_kingside(black) :-
    piece_at(5,8,black,king),
    piece_at(8,8,black,rook),
    clear_straight(5,8,8,8),
    \+ has_moved(black,king),
    \+ has_moved(black,rook,8,8),
    \+ in_check(black),
    \+ attacked(white,5,8),
    \+ attacked(white,6,8),
    \+ attacked(white,7,8).

% --- Kiểm tra nhập thành queenside ---
castle_queenside(white) :-
    piece_at(5,1,white,king),
    piece_at(1,1,white,rook),
    clear_straight(5,1,1,1),         % Kiểm tra các ô giữa vua và xe trống
    \+ has_moved(white,king),
    \+ has_moved(white,rook,1,1),
    \+ in_check(white),
    \+ attacked(black,5,1),
    \+ attacked(black,4,1),
    \+ attacked(black,3,1).

castle_queenside(black) :-
    piece_at(5,8,black,king),
    piece_at(1,8,black,rook),
    clear_straight(5,8,1,8),
    \+ has_moved(black,king),
    \+ has_moved(black,rook,1,8),
    \+ in_check(black),
    \+ attacked(white,5,8),
    \+ attacked(white,4,8),
    \+ attacked(white,3,8).