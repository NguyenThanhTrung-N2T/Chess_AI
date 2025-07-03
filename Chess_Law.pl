% Chess_Law.pl

% --- Declare dynamic ---
:- consult('Chess_Helper.pl'). % Helper nên được consult trước nếu Law dùng vị từ từ Helper
% :- consult('Chess_AI.pl'). % AI là module riêng, không nên là dependency của Law
:- dynamic piece_at/4.
:- dynamic last_move/4.
% Mark after king/rook moved
:- dynamic king_moved/1.
:- dynamic rook_moved/2.
% Count halfmove clock (Draw by 50 moves)
:- dynamic halfmove_clock/1.
halfmove_clock(0). % Count halfmove (+1 when moved)

:- dynamic board_history/1.

% Save history stack
board_history([]).

last_move(0,0,0,0). % Fact to save lastmove to check en_passant


% --- Pawn ---
pawn_dir(white, 1, 2).
pawn_dir(black, -1, 7).

pawn_move(Color, Col, Row, Col, Row2) :-
    pawn_dir(Color, Dir, StartRow),
    (
        % Đi 1 bước
        (Row2 is Row + Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row2, _, _))
        ;
        % Đi 2 bước: phải ở hàng xuất phát, cả ô giữa và ô đích đều trống
        (Row =:= StartRow, Row2 is Row + 2*Dir, Row2 >= 1, Row2 =< 8,
            RowMid is Row + Dir,
            \+ piece_at(Col, RowMid, _, _),
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
    Row3 =:= Row1 - 2*OtherDir,
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
    (
        (C1 =:= C2, clear_straight(C1, R1, C2, R2));
        (R1 =:= R2, clear_straight(C1, R1, C2, R2));
        (abs(C1 - C2) =:= abs(R1 - R2), clear_diagonal(C1, R1, C2, R2))
    ),
    ( 
        \+ piece_at(C2, R2, _, _)
        ;
        (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, queen), Color1 \= Color2)
    ).


% --- King ---
king_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1, abs(R1 - R2) =< 1,
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, king), Color1 \= Color2)).

% --- Rule to check legal moves ---
legal_move(pawn, Color, C1, R1, C2, R2) :-
    (   pawn_move(Color, C1, R1, C2, R2) ;
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
legal_move(king, Color, C1, R1, C2, R2) :-
    ( castle_king_side(Color, C1, R1, C2, R2) ; 
    castle_queen_side(Color, C1, R1, C2, R2) ; 
    king_move(C1, R1, C2, R2)),
    not_same_color(C2, R2, Color).

% --- Whether a king is in check --- (not bad)
in_check(Color) :-
    opponent_color(Color, OpponentColor),
    piece_at(KingC, KingR, Color, king),
    piece_at(C, R, OpponentColor, Piece),
    is_attacking(Piece, OpponentColor, C, R, KingC, KingR),
    !.

% --- Stalemate (king cannot go to anywhere except the current square) ---
stalemate(Color) :-
    \+ in_check(Color),
    \+ has_legal_move(Color).

% --- Draw by 50 moves rule ---
draw_by_fifty_moves :-
    halfmove_clock(N),
    N >= 100. % 100 nửa nước = 50 nước đầy đủ

% --- Draw by 3 fold repetition rule ---
draw_by_threefold_repetition :-
    snapshot(Current),
    count_occurrences(Current, Count),
    Count >= 3.

% --- Return color of a cell ---
square_color(C, R, white) :- Mod is (C + R) mod 2, Mod =:= 0.
square_color(C, R, black) :- Mod is (C + R) mod 2, Mod =:= 1.

% --- Find all legal moves ---
% all_legal_moves_for_piece(+Piece, +Color, +C1, +R1, -ListOfTargetSquares)
% ListOfTargetSquares will be a list of (C2, R2) tuples
all_legal_moves_for_piece(Piece, Color, C1, R1, TargetSquares) :-
    findall((C2, R2),
            (   between(1, 8, C2),
                between(1, 8, R2),
                (C1 \= C2 ; R1 \= R2), % Has to be a move, not standstill
                legal_move(Piece, Color, C1, R1, C2, R2), % Whether it is a legal move or no
                \+ causes_check(Piece, Color, C1, R1, C2, R2) % Move cannot lead its king in check
            ),
            TargetSquares).

% --- Get the list of all pieces except the king ---
get_remaining_pieces(RemainingPieces) :-
    findall((C, R, Color, Piece), (piece_at(C, R, Color, Piece), Piece \= king), RemainingPieces).

% --- Not enough piece to checkmate ---
insufficient_material :-
    get_remaining_pieces([]).  % Endgame with only Kings

insufficient_material :-
    get_remaining_pieces([(_, _, _, knight)]).  % Endgame with Kings and Knight

insufficient_material :-
    get_remaining_pieces([(_, _, _, bishop)]).  % Endgame with Kings and Bishop

% --- In case 2 bishops in the same color square ---
insufficient_material :-
    get_remaining_pieces([(C1, R1, _, bishop), (C2, R2, _, bishop)]),
    square_color(C1, R1, Color1),
    square_color(C2, R2, Color2),
    Color1 = Color2.  % Same color square -> Draw

% --- Checkmate ---
checkmate(Color) :-
    in_check(Color),
    \+ has_legal_move(Color).

% --- Any legal moves left ---
has_legal_move(Color) :-
    piece_at(C1, R1, Color, Piece),
    between(1, 8, C2),
    between(1, 8, R2),
    legal_move(Piece, Color, C1, R1, C2, R2),
    \+ causes_check(Piece, Color, C1, R1, C2, R2),
    !. % Stop if find one

% --- Whether moving a piece can lead the king in check or not ---
causes_check(Piece, Color, C1, R1, C2, R2) :-
    % Save the current last_move
    last_move(LMC1, LMR1, LMC2, LMR2),

    % Save the taken piece
    (piece_at(C2, R2, CapturedColor, CapturedPiece) ->
        HasCapture = true ; (HasCapture = false, CapturedColor = none, CapturedPiece = none)),

    % Temporarily move the piece
    retract(piece_at(C1, R1, Color, Piece)),
    (HasCapture -> retract(piece_at(C2, R2, CapturedColor, CapturedPiece)) ; true),
    assertz(piece_at(C2, R2, Color, Piece)),

    % Whether king is in check or no
    (in_check(Color) -> CheckResult = true ; CheckResult = false),

    % Undo a move
    retract(piece_at(C2, R2, Color, Piece)),
    (HasCapture -> assertz(piece_at(C2, R2, CapturedColor, CapturedPiece)) ; true),
    assertz(piece_at(C1, R1, Color, Piece)),

    % Bring the last_move back
    retractall(last_move(_, _, _, _)),
    assertz(last_move(LMC1, LMR1, LMC2, LMR2)),

    % Return
    CheckResult.

% --- Move piece ---
move_piece(Piece, Color, C1, R1, C2, R2) :-
    legal_move(Piece, Color, C1, R1, C2, R2),

    % Whether a piece is captured or no
    (piece_at(C2, R2, _, _) -> Captured = true ; Captured = false),

    % Move piece
    retract(piece_at(C1, R1, Color, Piece)),

    (   % If it is an en passant
        (Piece = pawn, en_passant(Color, C1, R1, C2, R2))
    ->  % Remove the captured pawn
        pawn_dir(Color, Dir, _),
        RowPawn is R2 - Dir,
        retract(piece_at(C2, RowPawn, _, pawn)),
        CapturedFlag = true
    ;   % If it is not an en passant
        (Captured -> retract(piece_at(C2, R2, _, _)) ; true),
        CapturedFlag = Captured
    ),

    assertz(piece_at(C2, R2, Color, Piece)),

    (\+ in_check(Color)),

    retractall(last_move(_,_,_,_)),
    assertz(last_move(C1, R1, C2, R2)),

    % Mark if the rook has moved
    % Only mark if the rook has moved from its initial position and flag is not set
    (Piece = rook,
        ( (Color = white, R1 = 1) ; (Color = black, R1 = 8) ), % Rook at the first row
        ( C1 = 1 ; C1 = 8 ) -> % Initial column (A or H)
            ( \+ rook_moved(Color, C1) -> % If the flag for the rook at column C1 is not set
                assertz(rook_moved(Color, C1))
            ; true )
    ; true ), % Do nothing if it is not a rook or not from initial position or is already set
    % Mark if the king has moved
    (Piece = king -> 
        (retractall(king_moved(Color)), assertz(king_moved(Color))),
        (abs(C2 - C1) =:= 2 ->  % Castle
            (C2 > C1 ->
                RookColOld is 8, RookColNew is 6
            ;
                RookColOld is 1, RookColNew is 4
            ),
            retract(piece_at(RookColOld, R1, Color, rook)),
            assertz(piece_at(RookColNew, R1, Color, rook))
        ; true)
    ; true),

    % Update the halfmove_clock
    (
        (Piece = pawn ; CapturedFlag = true) ->
            retractall(halfmove_clock(_)),
            assertz(halfmove_clock(0))
        ;
            (retract(halfmove_clock(N)) -> true ; N = 0),
            N1 is N + 1,
            retractall(halfmove_clock(_)),
            assertz(halfmove_clock(N1))
    ),

    % Update the board history to check 3 fold repetition
    push_board_history,


    !.
