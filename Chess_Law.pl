% Chess_Law.pl

% abs/2 cho SWI-Prolog
abs(X, Y) :- X >= 0, Y is X; X < 0, Y is -X.

% --- Pawn ---
% pawn_move(Color, ColFrom, RowFrom, ColTo, RowTo)
pawn_move(Color, Col, Row, Col, Row2) :-
    pawn_dir(Color, Dir, StartRow),
    (
        (Row2 is Row + Dir, Row2 >= 1, Row2 =< 8); % đi thẳng 1 ô
        (Row =:= StartRow, Row2 is Row + 2*Dir, Row2 >= 1, Row2 =< 8) % đi thẳng 2 ô từ vị trí xuất phát
    ),
    Col >= 1, Col =< 8.

pawn_dir(white, 1, 2).
pawn_dir(black, -1, 7).

% --- Knight ---
% knight_move(ColFrom, RowFrom, ColTo, RowTo)
knight_move(C1, R1, C2, R2) :-
    (
        (abs(C1 - C2) =:= 2, abs(R1 - R2) =:= 1);
        (abs(C1 - C2) =:= 1, abs(R1 - R2) =:= 2)
    ),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8.

% --- Bishop ---
% bishop_move(ColFrom, RowFrom, ColTo, RowTo)
bishop_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    C1 \= C2,
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8.

% --- Rook ---
% rook_move(ColFrom, RowFrom, ColTo, RowTo)
rook_move(C1, R1, C2, R2) :-
    (C1 =:= C2; R1 =:= R2),
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8.

% --- Queen ---
% queen_move(ColFrom, RowFrom, ColTo, RowTo)
queen_move(C1, R1, C2, R2) :-
    bishop_move(C1, R1, C2, R2);
    rook_move(C1, R1, C2, R2).

% --- King ---
% king_move(ColFrom, RowFrom, ColTo, RowTo)
king_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1, abs(R1 - R2) =< 1,
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8.