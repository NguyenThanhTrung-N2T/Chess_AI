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


