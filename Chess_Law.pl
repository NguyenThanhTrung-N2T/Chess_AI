% Chess_Law.pl

% --- Khai báo dynamic ---
:- consult('Chess_Helper.pl').
:- dynamic piece_at/4.

% --- Pawn ---
pawn_dir(white, 1, 2).
pawn_dir(black, -1, 7).

pawn_move(Color, Col, Row, Col, Row2) :-
    pawn_dir(Color, Dir, StartRow),
    (
        (Row2 is Row + Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row2, _, _))
        ;
        (Row =:= StartRow, Row2 is Row + 2*Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row + Dir, _, _),
            \+ piece_at(Col, Row2, _, _))
    ),
    Col >= 1, Col =< 8.

pawn_capture(Color, Col1, Row1, Col2, Row2) :-
    pawn_dir(Color, Dir, _),
    Row2 is Row1 + Dir,
    (Col2 is Col1 + 1; Col2 is Col1 - 1),
    Col2 >= 1, Col2 =< 8,
    Row2 >= 1, Row2 =< 8,
    piece_at(Col2, Row2, OtherColor, _),
    Color \= OtherColor.

en_passant(Color, Col1, Row1, Col2, Row2) :-
    pawn_dir(Color, Dir, _),
    (Color = white -> Row1 =:= 5 ; Row1 =:= 4),
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

% --- Hàm di chuyển quân cờ tổng quát ---
move_piece(Piece, Color, C1, R1, C2, R2) :-
    legal_move(Piece, Color, C1, R1, C2, R2),
    retract(piece_at(C1, R1, Color, Piece)),
    (retract(piece_at(C2, R2, _, _)); true),
    assertz(piece_at(C2, R2, Color, Piece)),
    (\+ in_check(Color)), 
    !.