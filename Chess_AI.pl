:- consult('Chess_Helper.pl').
:- consult('Chess_Law.pl').

% --- Declare dynamic predicates for transposition table ---
:- dynamic transposition/5. % transposition(PositionKey, Depth, Score, NodeType, BestMove)
% NodeType: exact, lower_bound, upper_bound

% --- Piece values ---
piece_value(pawn, 100).
piece_value(knight, 300).
piece_value(bishop, 300).
piece_value(rook, 500).
piece_value(queen, 900).
piece_value(king, 10000).

% --- Game phase determination ---
game_phase(Color, Phase) :-
    game_phase_score(Color, MyScore),
    game_phase_score(black, BlackScore),
    game_phase_score(white, WhiteScore),
    TotalPhase is BlackScore + WhiteScore,
    MaxPhase is 24, % 4 queens, 4 rooks, 4 knights, 4 bishops
    (TotalPhase >= 18 -> Phase = opening;
     TotalPhase =< 6 -> Phase = endgame;
     Phase = midgame).

% --- Piece-square tables (inspired by SebLagues Chess-V1-Unity) ---
% Values are scaled to match material values (approximately in centipawns)
% For white pieces; black pieces are mirrored
% --- Piece-square tables (values for white; black is mirrored) ---

% --- Opening book for first few moves ---
% opening_book(Color, MoveNumber, Piece, FromCol, FromRow, ToCol, ToRow)
% MoveNumber is the full move number (white and black move pair)
% Move 1 (White: 1. e4, 1. d4, 1. c4; Black: 1...e5, 1...d5, 1...c5)
opening_book(white, 1, pawn, 5, 2, 5, 4). % 1. e4
opening_book(white, 1, pawn, 4, 2, 4, 4). % 1. d4
opening_book(white, 1, pawn, 3, 2, 3, 4). % 1. c4
opening_book(black, 1, pawn, 5, 7, 5, 5). % 1...e5 (response to e4)
opening_book(black, 1, pawn, 4, 7, 4, 5). % 1...d5 (response to d4)
opening_book(black, 1, pawn, 3, 7, 3, 5). % 1...c5 (Sicilian, response to e4)
% Move 2 (White: 2. Nf3, 2. Nc3, 2. Bc4; Black: 2...Nf6, 2...Nc6, 2...d5)
opening_book(white, 2, knight, 7, 1, 6, 3). % 2. Nf3
opening_book(white, 2, knight, 3, 1, 3, 3). % 2. Nc3
opening_book(white, 2, bishop, 3, 1, 3, 4). % 2. Bc4
opening_book(black, 2, knight, 7, 8, 6, 6). % 2...Nf6
opening_book(black, 2, knight, 3, 8, 3, 6). % 2...Nc6
opening_book(black, 2, pawn, 4, 7, 4, 5). % 2...d5
% Move 3 (White: 3. Bc4, 3. Bb5, 3. d4; Black: 3...Bc5, 3...Bb4, 3...e6)
opening_book(white, 3, bishop, 3, 1, 3, 4). % 3. Bc4
opening_book(white, 3, bishop, 2, 1, 2, 5). % 3. Bb5
opening_book(white, 3, pawn, 4, 2, 4, 4). % 3. d4
opening_book(black, 3, bishop, 3, 8, 3, 5). % 3...Bc5
opening_book(black, 3, bishop, 2, 8, 2, 4). % 3...Bb4
opening_book(black, 3, pawn, 5, 7, 5, 6). % 3...e6
% Move 4 (Black: 4...Nf6, 4...d6, 4...g6; continuing Sicilian or other lines)
opening_book(black, 4, knight, 7, 8, 6, 6). % 4...Nf6
opening_book(black, 4, pawn, 4, 7, 4, 6). % 4...d6
opening_book(black, 4, pawn, 7, 7, 7, 6). % 4...g6
% Move 5 (Black: 5...Bg7, 5...a6, 5...e6)
opening_book(black, 5, bishop, 7, 8, 7, 4). % 5...Bg7
opening_book(black, 5, pawn, 1, 7, 1, 6). % 5...a6
opening_book(black, 5, pawn, 5, 7, 5, 6). % 5...e6

% --- Get current move number ---
% Move number is based on board history length (each entry is one ply)
get_move_number(MoveNumber) :-
    board_history(History),
    length(History, PlyCount),
    MoveNumber is PlyCount // 2 + 1.

% Pawn table: Favor center, penalize rank 8 (no promotion in eval)
pawn_table(1, 1, 0). pawn_table(2, 1, 0). pawn_table(3, 1, 0). pawn_table(4, 1, 0).
pawn_table(5, 1, 0). pawn_table(6, 1, 0). pawn_table(7, 1, 0). pawn_table(8, 1, 0).
pawn_table(1, 2, 5). pawn_table(2, 2, 10). pawn_table(3, 2, 10). pawn_table(4, 2, 20).
pawn_table(5, 2, 20). pawn_table(6, 2, 10). pawn_table(7, 2, 10). pawn_table(8, 2, 5).
pawn_table(1, 3, 5). pawn_table(2, 3, 10). pawn_table(3, 3, 20). pawn_table(4, 3, 30).
pawn_table(5, 3, 30). pawn_table(6, 3, 20). pawn_table(7, 3, 10). pawn_table(8, 3, 5).
pawn_table(1, 4, 0). pawn_table(2, 4, 0). pawn_table(3, 4, 20). pawn_table(4, 4, 40).
pawn_table(5, 4, 40). pawn_table(6, 4, 20). pawn_table(7, 4, 0). pawn_table(8, 4, 0).
pawn_table(1, 5, -10). pawn_table(2, 5, -10). pawn_table(3, 5, 0). pawn_table(4, 5, 20).
pawn_table(5, 5, 20). pawn_table(6, 5, 0). pawn_table(7, 5, -10). pawn_table(8, 5, -10).
pawn_table(1, 6, -20). pawn_table(2, 6, -20). pawn_table(3, 6, -10). pawn_table(4, 6, 0).
pawn_table(5, 6, 0). pawn_table(6, 6, -10). pawn_table(7, 6, -20). pawn_table(8, 6, -20).
pawn_table(1, 7, -30). pawn_table(2, 7, -30). pawn_table(3, 7, -30). pawn_table(4, 7, -30).
pawn_table(5, 7, -30). pawn_table(6, 7, -30). pawn_table(7, 7, -30). pawn_table(8, 7, -30).
pawn_table(1, 8, -50). pawn_table(2, 8, -50). pawn_table(3, 8, -50). pawn_table(4, 8, -50).
pawn_table(5, 8, -50). pawn_table(6, 8, -50). pawn_table(7, 8, -50). pawn_table(8, 8, -50).

% Knight table: Favor center
knight_table(1, 1, -50). knight_table(2, 1, -40). knight_table(3, 1, -30). knight_table(4, 1, -30).
knight_table(5, 1, -30). knight_table(6, 1, -30). knight_table(7, 1, -40). knight_table(8, 1, -50).
knight_table(1, 2, -40). knight_table(2, 2, -20). knight_table(3, 2, 0). knight_table(4, 2, 0).
knight_table(5, 2, 0). knight_table(6, 2, 0). knight_table(7, 2, -20). knight_table(8, 2, -40).
knight_table(1, 3, -30). knight_table(2, 3, 0). knight_table(3, 3, 10). knight_table(4, 3, 15).
knight_table(5, 3, 15). knight_table(6, 3, 10). knight_table(7, 3, 0). knight_table(8, 3, -30).
knight_table(1, 4, -30). knight_table(2, 4, 0). knight_table(3, 4, 15). knight_table(4, 4, 20).
knight_table(5, 4, 20). knight_table(6, 4, 15). knight_table(7, 4, 0). knight_table(8, 4, -30).
knight_table(1, 5, -30). knight_table(2, 5, 0). knight_table(3, 5, 15). knight_table(4, 5, 20).
knight_table(5, 5, 20). knight_table(6, 5, 15). knight_table(7, 5, 0). knight_table(8, 5, -30).
knight_table(1, 6, -30). knight_table(2, 6, 0). knight_table(3, 6, 10). knight_table(4, 6, 15).
knight_table(5, 6, 15). knight_table(6, 6, 10). knight_table(7, 6, 0). knight_table(8, 6, -30).
knight_table(1, 7, -40). knight_table(2, 7, -20). knight_table(3, 7, 0). knight_table(4, 7, 0).
knight_table(5, 7, 0). knight_table(6, 7, 0). knight_table(7, 7, -20). knight_table(8, 7, -40).
knight_table(1, 8, -50). knight_table(2, 8, -40). knight_table(3, 8, -30). knight_table(4, 8, -30).
knight_table(5, 8, -30). knight_table(6, 8, -30). knight_table(7, 8, -40). knight_table(8, 8, -50).

% Bishop table: Favor long diagonals
bishop_table(1, 1, -20). bishop_table(2, 1, -10). bishop_table(3, 1, -10). bishop_table(4, 1, -10).
bishop_table(5, 1, -10). bishop_table(6, 1, -10). bishop_table(7, 1, -10). bishop_table(8, 1, -20).
bishop_table(1, 2, -10). bishop_table(2, 2, 0). bishop_table(3, 2, 0). bishop_table(4, 2, 0).
bishop_table(5, 2, 0). bishop_table(6, 2, 0). bishop_table(7, 2, 0). bishop_table(8, 2, -10).
bishop_table(1, 3, -10). bishop_table(2, 3, 0). bishop_table(3, 3, 5). bishop_table(4, 3, 5).
bishop_table(5, 3, 5). bishop_table(6, 3, 5). bishop_table(7, 3, 0). bishop_table(8, 3, -10).
bishop_table(1, 4, -10). bishop_table(2, 4, 0). bishop_table(3, 4, 5). bishop_table(4, 4, 10).
bishop_table(5, 4, 10). bishop_table(6, 4, 5). bishop_table(7, 4, 0). bishop_table(8, 4, -10).
bishop_table(1, 5, -10). bishop_table(2, 5, 0). bishop_table(3, 5, 5). bishop_table(4, 5, 10).
bishop_table(5, 5, 10). bishop_table(6, 5, 5). bishop_table(7, 5, 0). bishop_table(8, 5, -10).
bishop_table(1, 6, -10). bishop_table(2, 6, 0). bishop_table(3, 6, 5). bishop_table(4, 6, 5).
bishop_table(5, 6, 5). bishop_table(6, 6, 5). bishop_table(7, 6, 0). bishop_table(8, 6, -10).
bishop_table(1, 7, -10). bishop_table(2, 7, 0). bishop_table(3, 7, 0). bishop_table(4, 7, 0).
bishop_table(5, 7, 0). bishop_table(6, 7, 0). bishop_table(7, 7, 0). bishop_table(8, 7, -10).
bishop_table(1, 8, -20). bishop_table(2, 8, -10). bishop_table(3, 8, -10). bishop_table(4, 8, -10).
bishop_table(5, 8, -10). bishop_table(6, 8, -10). bishop_table(7, 8, -10). bishop_table(8, 8, -20).

% Rook table: Favor 7th rank for white
rook_table(1, 1, 0). rook_table(2, 1, 0). rook_table(3, 1, 0). rook_table(4, 1, 0).
rook_table(5, 1, 0). rook_table(6, 1, 0). rook_table(7, 1, 0). rook_table(8, 1, 0).
rook_table(1, 2, 5). rook_table(2, 2, 5). rook_table(3, 2, 5). rook_table(4, 2, 5).
rook_table(5, 2, 5). rook_table(6, 2, 5). rook_table(7, 2, 5). rook_table(8, 2, 5).
rook_table(1, 3, 0). rook_table(2, 3, 0). rook_table(3, 3, 0). rook_table(4, 3, 0).
rook_table(5, 3, 0). rook_table(6, 3, 0). rook_table(7, 3, 0). rook_table(8, 3, 0).
rook_table(1, 4, 0). rook_table(2, 4, 0). rook_table(3, 4, 0). rook_table(4, 4, 0).
rook_table(5, 4, 0). rook_table(6, 4, 0). rook_table(7, 4, 0). rook_table(8, 4, 0).
rook_table(1, 5, 0). rook_table(2, 5, 0). rook_table(3, 5, 0). rook_table(4, 5, 0).
rook_table(5, 5, 0). rook_table(6, 5, 0). rook_table(7, 5, 0). rook_table(8, 5, 0).
rook_table(1, 6, 0). rook_table(2, 6, 0). rook_table(3, 6, 0). rook_table(4, 6, 0).
rook_table(5, 6, 0). rook_table(6, 6, 0). rook_table(7, 6, 0). rook_table(8, 6, 0).
rook_table(1, 7, 10). rook_table(2, 7, 10). rook_table(3, 7, 10). rook_table(4, 7, 10).
rook_table(5, 7, 10). rook_table(6, 7, 10). rook_table(7, 7, 10). rook_table(8, 7, 10).
rook_table(1, 8, 0). rook_table(2, 8, 0). rook_table(3, 8, 0). rook_table(4, 8, 0).
rook_table(5, 8, 0). rook_table(6, 8, 0). rook_table(7, 8, 0). rook_table(8, 8, 0).

% Queen table: Favor center
queen_table(1, 1, -20). queen_table(2, 1, -10). queen_table(3, 1, -10). queen_table(4, 1, -10).
queen_table(5, 1, -10). queen_table(6, 1, -10). queen_table(7, 1, -10). queen_table(8, 1, -20).
queen_table(1, 2, -10). queen_table(2, 2, 0). queen_table(3, 2, 0). queen_table(4, 2, 0).
queen_table(5, 2, 0). queen_table(6, 2, 0). queen_table(7, 2, 0). queen_table(8, 2, -10).
queen_table(1, 3, -10). queen_table(2, 3, 0). queen_table(3, 3, 5). queen_table(4, 3, 5).
queen_table(5, 3, 5). queen_table(6, 3, 5). queen_table(7, 3, 0). queen_table(8, 3, -10).
queen_table(1, 4, -10). queen_table(2, 4, 0). queen_table(3, 4, 5). queen_table(4, 4, 10).
queen_table(5, 4, 10). queen_table(6, 4, 5). queen_table(7, 4, 0). queen_table(8, 4, -10).
queen_table(1, 5, -10). queen_table(2, 5, 0). queen_table(3, 5, 5). queen_table(4, 5, 10).
queen_table(5, 5, 10). queen_table(6, 5, 5). queen_table(7, 5, 0). queen_table(8, 5, -10).
queen_table(1, 6, -10). queen_table(2, 6, 0). queen_table(3, 6, 5). queen_table(4, 6, 5).
queen_table(5, 6, 5). queen_table(6, 6, 5). queen_table(7, 6, 0). queen_table(8, 6, -10).
queen_table(1, 7, -10). queen_table(2, 7, 0). queen_table(3, 7, 0). queen_table(4, 7, 0).
queen_table(5, 7, 0). queen_table(6, 7, 0). queen_table(7, 7, 0). queen_table(8, 7, -10).
queen_table(1, 8, -20). queen_table(2, 8, -10). queen_table(3, 8, -10). queen_table(4, 8, -10).
queen_table(5, 8, -10). queen_table(6, 8, -10). queen_table(7, 8, -10). queen_table(8, 8, -20).

% King opening table: Favor corners
king_opening_table(1, 1, 20). king_opening_table(2, 1, 10). king_opening_table(3, 1, 0). king_opening_table(4, 1, -10).
king_opening_table(5, 1, -10). king_opening_table(6, 1, 0). king_opening_table(7, 1, 10). king_opening_table(8, 1, 20).
king_opening_table(1, 2, 10). king_opening_table(2, 2, 0). king_opening_table(3, 2, -10). king_opening_table(4, 2, -20).
king_opening_table(5, 2, -20). king_opening_table(6, 2, -10). king_opening_table(7, 2, 0). king_opening_table(8, 2, 10).
king_opening_table(1, 3, 0). king_opening_table(2, 3, -10). king_opening_table(3, 3, -20). king_opening_table(4, 3, -30).
king_opening_table(5, 3, -30). king_opening_table(6, 3, -20). king_opening_table(7, 3, -10). king_opening_table(8, 3, 0).
king_opening_table(1, 4, -10). king_opening_table(2, 4, -20). king_opening_table(3, 4, -30). king_opening_table(4, 4, -40).
king_opening_table(5, 4, -40). king_opening_table(6, 4, -30). king_opening_table(7, 4, -20). king_opening_table(8, 4, -10).
king_opening_table(1, 5, -20). king_opening_table(2, 5, -30). king_opening_table(3, 5, -40). king_opening_table(4, 5, -50).
king_opening_table(5, 5, -50). king_opening_table(6, 5, -40). king_opening_table(7, 5, -30). king_opening_table(8, 5, -20).
king_opening_table(1, 6, -30). king_opening_table(2, 6, -40). king_opening_table(3, 6, -50). king_opening_table(4, 6, -60).
king_opening_table(5, 6, -60). king_opening_table(6, 6, -50). king_opening_table(7, 6, -40). king_opening_table(8, 6, -30).
king_opening_table(1, 7, -40). king_opening_table(2, 7, -50). king_opening_table(3, 7, -60). king_opening_table(4, 7, -70).
king_opening_table(5, 7, -70). king_opening_table(6, 7, -60). king_opening_table(7, 7, -50). king_opening_table(8, 7, -40).
king_opening_table(1, 8, -50). king_opening_table(2, 8, -60). king_opening_table(3, 8, -70). king_opening_table(4, 8, -80).
king_opening_table(5, 8, -80). king_opening_table(6, 8, -70). king_opening_table(7, 8, -60). king_opening_table(8, 8, -50).

% King endgame table: Favor center
king_endgame_table(1, 1, -10). king_endgame_table(2, 1, 0). king_endgame_table(3, 1, 10). king_endgame_table(4, 1, 20).
king_endgame_table(5, 1, 20). king_endgame_table(6, 1, 10). king_endgame_table(7, 1, 0). king_endgame_table(8, 1, -10).
king_endgame_table(1, 2, 0). king_endgame_table(2, 2, 10). king_endgame_table(3, 2, 20). king_endgame_table(4, 2, 30).
king_endgame_table(5, 2, 30). king_endgame_table(6, 2, 20). king_endgame_table(7, 2, 10). king_endgame_table(8, 2, 0).
king_endgame_table(1, 3, 10). king_endgame_table(2, 3, 20). king_endgame_table(3, 3, 30). king_endgame_table(4, 3, 40).
king_endgame_table(5, 3, 40). king_endgame_table(6, 3, 30). king_endgame_table(7, 3, 20). king_endgame_table(8, 3, 10).
king_endgame_table(1, 4, 20). king_endgame_table(2, 4, 30). king_endgame_table(3, 4, 40). king_endgame_table(4, 4, 50).
king_endgame_table(5, 4, 50). king_endgame_table(6, 4, 40). king_endgame_table(7, 4, 30). king_endgame_table(8, 4, 20).
king_endgame_table(1, 5, 20). king_endgame_table(2, 5, 30). king_endgame_table(3, 5, 40). king_endgame_table(4, 5, 50).
king_endgame_table(5, 5, 50). king_endgame_table(6, 5, 40). king_endgame_table(7, 5, 30). king_endgame_table(8, 5, 20).
king_endgame_table(1, 6, 10). king_endgame_table(2, 6, 20). king_endgame_table(3, 6, 30). king_endgame_table(4, 6, 40).
king_endgame_table(5, 6, 40). king_endgame_table(6, 6, 30). king_endgame_table(7, 6, 20). king_endgame_table(8, 6, 10).
king_endgame_table(1, 7, 0). king_endgame_table(2, 7, 10). king_endgame_table(3, 7, 20). king_endgame_table(4, 7, 30).
king_endgame_table(5, 7, 30). king_endgame_table(6, 7, 20). king_endgame_table(7, 7, 10). king_endgame_table(8, 7, 0).
king_endgame_table(1, 8, -10). king_endgame_table(2, 8, 0). king_endgame_table(3, 8, 10). king_endgame_table(4, 8, 20).
king_endgame_table(5, 8, 20). king_endgame_table(6, 8, 10). king_endgame_table(7, 8, 0). king_endgame_table(8, 8, -10).

% --- Game phase calculation ---
game_phase_score(Color, PhaseScore) :-
    findall(Value, (
        piece_at(_, _, Color, Piece),
        (Piece = pawn -> Value is 0;
         Piece = knight -> Value is 1;
         Piece = bishop -> Value is 1;
         Piece = rook -> Value is 2;
         Piece = queen -> Value is 4;
         Value is 0)
    ), PhaseValues),
    sum_list(PhaseValues, PhaseScore).

% --- Piece-square table evaluation ---
piece_position_value(Piece, Color, C, R, Value) :-
    (Color = white -> Row = R; Row is 9 - R),
    ( Piece = pawn   -> pawn_table(C, Row, Value)
    ; Piece = knight -> knight_table(C, Row, Value)
    ; Piece = bishop -> bishop_table(C, Row, Value)
    ; Piece = rook   -> rook_table(C, Row, Value)
    ; Piece = queen  -> queen_table(C, Row, Value)
    ; Piece = king   ->
        (game_phase(Color, Phase),
         (Phase = opening -> king_opening_table(C, Row, Value)
         ; Phase = endgame -> king_endgame_table(C, Row, Value)
         ; % Midgame interpolation
           game_phase_score(Color, MyScore), game_phase_score(black, BlackScore), game_phase_score(white, WhiteScore),
           TotalPhase is BlackScore + WhiteScore,
           MaxPhase is 24,
           OpeningWeight is (MaxPhase - TotalPhase) / MaxPhase,
           EndgameWeight is TotalPhase / MaxPhase,
           king_opening_table(C, Row, OpeningValue),
           king_endgame_table(C, Row, EndgameValue),
           Value is OpeningWeight * OpeningValue + EndgameWeight * EndgameValue
         )
        )
    ; Value = 0
    ).

% --- Pawn structure penalties ---
pawn_structure_penalty(Color, Penalty) :-
    findall(C, (piece_at(C, R, Color, pawn), piece_at(C, R2, Color, pawn), R \= R2), Cols),
    sort(Cols, UniqueCols),
    length(UniqueCols, NumUnique),
    length(Cols, Total),
    Penalty is -20 * (Total - NumUnique). % Penalty for doubled pawns

% --- Mobility evaluation ---
piece_mobility(Color, Piece, C, R, MobilityScore) :-
    all_legal_moves_for_piece(Piece, Color, C, R, Moves),
    length(Moves, Mobility),
    MobilityScore is Mobility * 5. % 5 centipawns per legal move

% --- Count developed minor pieces ---
count_developed_pieces(Color, Count) :-
    findall(Piece, (
        piece_at(C, R, Color, Piece),
        (Piece = knight; Piece = bishop),
        (Color = white -> R > 2; R < 7) % Knight or bishop moved beyond initial rank
    ), DevelopedPieces),
    length(DevelopedPieces, Count).

% --- Evaluate board ---
evaluate_board(Color, Score) :-
    % Material score
    findall(Value, (piece_at(_, _, Color, Piece), piece_value(Piece, Value)), MyMaterialValues),
    sum_list(MyMaterialValues, MyMaterialScore),
    opponent_color(Color, OppColor),
    findall(Value, (piece_at(_, _, OppColor, Piece), piece_value(Piece, Value)), OppMaterialValues),
    sum_list(OppMaterialValues, OppMaterialScore),
    MaterialScore is MyMaterialScore - OppMaterialScore,

    % Position score
    findall(PosValue, (
        piece_at(C, R, Color, Piece),
        piece_position_value(Piece, Color, C, R, PosValue)
    ), MyPositionValues),
    sum_list(MyPositionValues, MyPositionScore),
    findall(PosValue, (
        piece_at(C, R, OppColor, Piece),
        piece_position_value(Piece, OppColor, C, R, PosValue)
    ), OppPositionValues),
    sum_list(OppPositionValues, OppPositionScore),
    PositionScore is MyPositionScore - OppPositionScore,

    % Mobility score
    findall(Mobility, (
        piece_at(C, R, Color, Piece),
        piece_mobility(Color, Piece, C, R, Mobility)
    ), MyMobilityValues),
    sum_list(MyMobilityValues, MyMobilityScore),
    findall(Mobility, (
        piece_at(C, R, OppColor, Piece),
        piece_mobility(OppColor, Piece, C, R, Mobility)
    ), OppMobilityValues),
    sum_list(OppMobilityValues, OppMobilityScore),
    MobilityScore is MyMobilityScore - OppMobilityScore,

    % Pawn structure
    pawn_structure_penalty(Color, MyPawnPenalty),
    pawn_structure_penalty(OppColor, OppPawnPenalty),
    PawnStructureScore is MyPawnPenalty - OppPawnPenalty,

    % Penalty for early queen move
    get_move_number(MoveNumber),
    (piece_at(C, R, Color, queen), 
     MoveNumber =< 6, % Extended to move 6
     count_developed_pieces(Color, DevCount), 
     DevCount < 4, % Stricter: 4 minor pieces
     (Color = white -> R > 2; R < 7) -> % Queen moved beyond initial rank
        QueenPenalty is -100 % Increased penalty
    ; QueenPenalty is 0),
    opponent_color(Color, OppColor),
    (piece_at(C2, R2, OppColor, queen), 
     MoveNumber =< 6, 
     count_developed_pieces(OppColor, OppDevCount), 
     OppDevCount < 4,
     (OppColor = white -> R2 > 2; R2 < 7) ->
        OppQueenPenalty is -100
    ; OppQueenPenalty is 0),
    EarlyQueenPenalty is QueenPenalty - OppQueenPenalty,

    % Total score
    Score is MaterialScore + PositionScore + MobilityScore + PawnStructureScore + EarlyQueenPenalty.

% --- Move scoring for ordering ---
score_move(Piece, C1, R1, C2, R2, Color, Score) :-
    % Check for capture
    (piece_at(C2, R2, OppColor, CapturedPiece), OppColor \= Color ->
        piece_value(CapturedPiece, CaptureValue),
        CaptureScore is CaptureValue * 10; % Amplify capture value
        CaptureScore is 0
    ),
    % Check for promotion (pawn to rank 8 or 1)
    (Piece = pawn, (Color = white, R2 = 8; Color = black, R2 = 1) ->
        PromotionScore is 900; % Value of promoting to queen
        PromotionScore is 0
    ),
    % Base move score (small random component to break ties)
    BaseScore is 0,
    % Total score
    Score is CaptureScore + PromotionScore + BaseScore.

% --- Sort moves by score ---
sort_moves(Moves, Color, SortedMoves) :-
    findall((Score, Move),
            (member(Move, Moves),
             Move = (Piece, C1, R1, C2, R2),
             score_move(Piece, C1, R1, C2, R2, Color, Score)),
            ScoredMoves),
    sort(1, @>=, ScoredMoves, SortedScoredMoves),
    findall(Move, member((_, Move), SortedScoredMoves), SortedMoves).

% --- Generate position key (simplified, using sorted board snapshot) ---
position_key(Key) :-
    snapshot(Snapshot),
    term_to_atom(Snapshot, Key).

% --- All legal moves ---
all_legal_moves(Color, Moves) :-
    findall((Piece, C1, R1, C2, R2),
            (piece_at(C1, R1, Color, Piece),
             between(1, 8, C2),
             between(1, 8, R2),
             (C1 \= C2 ; R1 \= R2),
             legal_move(Piece, Color, C1, R1, C2, R2),
             \+ causes_check(Piece, Color, C1, R1, C2, R2)),
            RawMoves),
    sort(RawMoves, Moves).

% --- Simulate move ---
simulate_move(Piece, Color, C1, R1, C2, R2, Captured) :-
    (piece_at(C2, R2, OppColor, CapturedPiece), OppColor \= Color ->
        Captured = (C2, R2, OppColor, CapturedPiece),
        retract(piece_at(C2, R2, OppColor, CapturedPiece))
    ; Captured = none
    ),
    retract(piece_at(C1, R1, Color, Piece)),
    assertz(piece_at(C2, R2, Color, Piece)).

% --- Undo move ---
undo_move(Piece, Color, C1, R1, C2, R2, none) :-
    retract(piece_at(C2, R2, Color, Piece)),
    assertz(piece_at(C1, R1, Color, Piece)).

undo_move(Piece, Color, C1, R1, C2, R2, (C2, R2, OppColor, CapturedPiece)) :-
    retract(piece_at(C2, R2, Color, Piece)),
    assertz(piece_at(C2, R2, OppColor, CapturedPiece)),
    assertz(piece_at(C1, R1, Color, Piece)).

% --- Minimax with Alpha-Beta Pruning and Transposition Table ---
minimax(Color, 0, _Alpha, _Beta, Score, none) :- !,
    evaluate_board(Color, Score).

minimax(Color, Depth, Alpha, Beta, BestScore, BestMove) :-
    Depth > 0,
    % Check transposition table
    position_key(Key),
    (transposition(Key, Depth, StoredScore, NodeType, StoredMove),
     (NodeType = exact; (NodeType = lower_bound, StoredScore >= Beta); (NodeType = upper_bound, StoredScore =< Alpha)) ->
        BestScore = StoredScore,
        BestMove = StoredMove
    ;
        all_legal_moves(Color, Moves),
        (Moves == [] ->
            (in_check(Color) ->
                BestScore is -10000 - Depth,
                BestMove = none
            ; BestScore is 0,
              BestMove = none
            )
        ;
            % Sort moves for better pruning
            sort_moves(Moves, Color, SortedMoves),
            opponent_color(Color, OppColor),
            NextDepth is Depth - 1,
            InitialBestScore is -20001,
            InitialBestMove = none,
            process_moves(SortedMoves, Color, OppColor, NextDepth, Alpha, Beta, InitialBestScore, InitialBestMove, BestScore, BestMove),
            % Store in transposition table
            (BestScore >= Beta -> NodeType = lower_bound;
             BestScore =< Alpha -> NodeType = upper_bound;
             NodeType = exact),
            retractall(transposition(Key, Depth, _, _, _)),
            assertz(transposition(Key, Depth, BestScore, NodeType, BestMove))
        )
    ).

% --- Process moves ---
process_moves([], _PlayerColor, _OpponentColor, _CurrentDepth, _Alpha, _Beta, CurrentBestScoreSoFar, CurrentBestMoveSoFar, CurrentBestScoreSoFar, CurrentBestMoveSoFar).

process_moves([(Piece, C1, R1, C2, R2) | RestMoves], PlayerColor, OpponentColor, CurrentDepth, Alpha, Beta, CurrentBestScoreSoFar, CurrentBestMoveSoFar, FinalBestScore, FinalBestMove) :-
    CurrentMove = (Piece, C1, R1, C2, R2),
    simulate_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),
    NewAlphaForOpponent is -Beta,
    NewBetaForOpponent is -Alpha,
    minimax(OpponentColor, CurrentDepth, NewAlphaForOpponent, NewBetaForOpponent, OpponentRawScore, _OpponentMove),
    undo_move(Piece, PlayerColor, C1, R1, C2, R2, Captured),
    ValueForThisMove is -OpponentRawScore,
    (ValueForThisMove > CurrentBestScoreSoFar ->
        UpdatedBestScore = ValueForThisMove,
        UpdatedBestMove = CurrentMove,
        UpdatedAlpha = max(Alpha, UpdatedBestScore)
    ; UpdatedBestScore = CurrentBestScoreSoFar,
      UpdatedBestMove = CurrentBestMoveSoFar,
      UpdatedAlpha = Alpha
    ),
    (UpdatedBestScore >= Beta ->
        FinalBestScore = UpdatedBestScore,
        FinalBestMove = UpdatedBestMove
    ; process_moves(RestMoves, PlayerColor, OpponentColor, CurrentDepth, UpdatedAlpha, Beta, UpdatedBestScore, UpdatedBestMove, FinalBestScore, FinalBestMove)
    ).

% --- Initial minimax call ---
minimax(Color, Depth, Score, Move) :-
    minimax(Color, Depth, -20001, 20001, Score, Move).

% --- Random move ---
random_move(Color, Piece, C1, R1, C2, R2) :-
    all_legal_moves(Color, Moves),
    (Moves == [] -> fail;
     random_member((Piece, C1, R1, C2, R2), Moves)
    ).
% --- Find best move ---
find_best_move(Color, Level, FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2) :-
    all_legal_moves(Color, Moves),
    (Moves == [] -> fail;
     % Check if we should use the opening book
     get_move_number(MoveNumber),
     MoveNumber =< 3, % Use opening book for first 3 full moves
     findall((Piece, C1, R1, C2, R2),
             opening_book(Color, MoveNumber, Piece, C1, R1, C2, R2),
             OpeningMoves),
     OpeningMoves \= [],
     % Select a random move from the opening book
     random_member((FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2), OpeningMoves),
     legal_move(FinalPiece, Color, FinalC1, FinalR1, FinalC2, FinalR2),
     \+ causes_check(FinalPiece, Color, FinalC1, FinalR1, FinalC2, FinalR2)
    ;
     % Fallback to original logic if no opening book move or beyond move 3
     (Level = easy ->
         random_member((FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2), Moves)
     ; % Determine depth based on game phase and level
       game_phase(Color, Phase),
       (Level = medium ->
           (Phase = opening -> Depth = 2;
            Phase = endgame -> Depth = 4;
            Depth = 3)
         ; % Level = hard
           (Phase = opening -> Depth = 2;
            Phase = endgame -> Depth = 5;
            Depth = 4)
       ),
       minimax(Color, Depth, _Score, BestMoveFound),
       (BestMoveFound = none -> fail;
        BestMoveFound = (FinalPiece, FinalC1, FinalR1, FinalC2, FinalR2))
     )
    ).
