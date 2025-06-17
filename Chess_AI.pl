:- dynamic best_move/6.

% --- Tính heuristic đơn giản dựa vào giá trị quân cờ ---
piece_value(pawn, 1).
piece_value(knight, 3).
piece_value(bishop, 3).
piece_value(rook, 5).
piece_value(queen, 9).
piece_value(king, 1000).  % Vua có giá trị cực lớn để tránh mất

% --- Hàm tính điểm toàn bàn cờ ---
evaluate_board(Color, Score) :-
    findall(Value, (piece_at(_, _, Color, Piece), piece_value(Piece, Value)), MyValues),
    findall(Value, (piece_at(_, _, OppColor, Piece), piece_value(Piece, Value), opposite_color(Color, OppColor)), OppValues),
    sum_list(MyValues, MyScore),
    sum_list(OppValues, OppScore),
    Score is MyScore - OppScore.

% --- Tìm tất cả nước đi hợp lệ ---
all_legal_moves(Color, Moves) :-
    findall((Piece, C1, R1, C2, R2), (piece_at(C1, R1, Color, Piece), legal_move(Piece, Color, C1, R1, C2, R2)), Moves).

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

% --- Minimax cơ bản ---
minimax(Color, Depth, BestScore, BestMove) :-
    minimax(Color, Depth, -10000, 10000, BestScore, BestMove).

minimax(Color, 0, Score, _) :-
    evaluate_board(Color, Score).

minimax(Color, Depth, Alpha, Beta, BestScore, BestMove) :-
    all_legal_moves(Color, Moves),
    Moves \= [],
    NextDepth is Depth - 1,
    findall(Score-Move,
        (
            member((Piece, C1, R1, C2, R2), Moves),
            simulate_move(Piece, Color, C1, R1, C2, R2, Captured),
            opposite_color(Color, OppColor),
            minimax(OppColor, NextDepth, OppScore, _),
            Score is -OppScore,
            undo_move(Piece, Color, C1, R1, C2, R2, Captured),
            Move = (Piece, C1, R1, C2, R2)
        ), ScoredMoves),
    sort(ScoredMoves, SortedMoves),
    reverse(SortedMoves, [BestScore-BestMove | _]).

% --- Tìm nước đi tốt nhất theo cấp độ ---
find_best_move(Color, Level, Piece, C1, R1, C2, R2) :-
    (Level = easy -> random_move(Color, Piece, C1, R1, C2, R2);
     Level = medium -> minimax(Color, 2, _, (Piece, C1, R1, C2, R2));
     Level = hard -> minimax(Color, 4, _, (Piece, C1, R1, C2, R2))).

% --- Nước đi ngẫu nhiên ---
random_move(Color, Piece, C1, R1, C2, R2) :-
    all_legal_moves(Color, Moves),
    random_member((Piece, C1, R1, C2, R2), Moves).

% --- Đối màu ---
opposite_color(white, black).
opposite_color(black, white).
