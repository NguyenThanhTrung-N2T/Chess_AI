% Chess_Law.pl

% --- Khai báo dynamic ---
:- dynamic piece_at/4.
:- dynamic board_history/1.

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
    (pawn_move(Color, C1, R1, C2, R2) ; pawn_capture(Color, C1, R1, C2, R2)),
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
    legal_move(Piece, Color, C1, R1, C2, R2),
    save_board_state,  % Lưu trạng thái trước khi đi
    retract(piece_at(C1, R1, Color, Piece)),
    (retract(piece_at(C2, R2, _, _)); true), % Nếu có quân bị ăn
    assertz(piece_at(C2, R2, Color, Piece)).
    