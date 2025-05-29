% Chess_Law.pl

% --- Hàm hỗ trợ ---
% trả về giá trị tuyệt đối của số x 
% abs/2 cho SWI-Prolog nếu chưa có sẵn
abs(X, Y) :- X >= 0, Y is X; X < 0, Y is -X.

% --- Pawn ---
% cho biết hướng di chuyển của quân tốt theo color và hàng xuất phát
% pawn_dir(Color, Direction, StartRow) 
pawn_dir(white, 1, 2).
pawn_dir(black, -1, 7).

% Di chuyển thẳng (không ăn) của quân tốt
% pawn_move(Color, ColFrom, RowFrom, ColTo, RowTo)
pawn_move(Color, Col, Row, Col, Row2) :-
    pawn_dir(Color, Dir, StartRow),
    (        (Row2 is Row + Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row2, _, _)  % phải trống
        ); % đi thẳng 1 ô
        (Row =:= StartRow, Row2 is Row + 2*Dir, Row2 >= 1, Row2 =< 8,
            \+ piece_at(Col, Row + Dir, _, _),
            \+ piece_at(Col, Row2, _, _)  % cả 2 ô đều trống
        ) % đi 2 ô từ vị trí xuất phát
    ),
    Col >= 1, Col =< 8.

% Ăn chéo của quân tốt
% pawn_capture(Color, ColFrom, RowFrom, ColTo, RowTo)
pawn_capture(Color, Col1, Row1, Col2, Row2) :-
    pawn_dir(Color, Dir, _),
    Row2 is Row1 + Dir,
    (Col2 is Col1 + 1; Col2 is Col1 - 1),
    Col2 >= 1, Col2 =< 8,
    Row2 >= 1, Row2 =< 8,
    piece_at(Col2, Row2, OtherColor, _),
    Color \= OtherColor.

% --- Knight ---
% Di chuyển của quân 
% knight_move(ColFrom, RowFrom, ColTo, RowTo)
knight_move(C1, R1, C2, R2) :-
    (
        (abs(C1 - C2) =:= 2, abs(R1 - R2) =:= 1);
        (abs(C1 - C2) =:= 1, abs(R1 - R2) =:= 2)
    ),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, knight), Color1 \= Color2)).

% --- Bishop ---
% Di chuyển của quân tượng
% bishop_move(ColFrom, RowFrom, ColTo, RowTo)
bishop_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    C1 \= C2,
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_diagonal(C1, R1, C2, R2),
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, bishop), Color1 \= Color2)).

% --- Rook ---
% Di chuyển của quân xe
% rook_move(ColFrom, RowFrom, ColTo, RowTo)
rook_move(C1, R1, C2, R2) :-
    (C1 =:= C2; R1 =:= R2),
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    clear_straight(C1, R1, C2, R2),
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, rook), Color1 \= Color2)).

% --- Queen ---
% Di chuyển của quân hậu
% queen_move(ColFrom, RowFrom, ColTo, RowTo)
queen_move(C1, R1, C2, R2) :-
    (bishop_move(C1, R1, C2, R2);
     rook_move(C1, R1, C2, R2)).

% --- King ---
% Di chuyển của quân vua
% king_move(ColFrom, RowFrom, ColTo, RowTo)
king_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1, abs(R1 - R2) =< 1,
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, king), Color1 \= Color2)).


% --- Hỗ trợ kiểm tra đường đi ---
% clear_straight/4: không có quân cản đường theo hàng/cột
clear_straight(C1, R1, C2, R2) :-
    (C1 =:= C2 ->
        between_min_max(R1, R2, Min, Max),
        \+ (between(Min, Max, R), R \= R1, R \= R2, piece_at(C1, R, _, _))
    ;
     R1 =:= R2 ->
        between_min_max(C1, C2, Min, Max),
        \+ (between(Min, Max, C), C \= C1, C \= C2, piece_at(C, R1, _, _))
    ).

% clear_diagonal/4: không có quân cản đường theo đường chéo
clear_diagonal(C1, R1, C2, R2) :-
    abs(C1 - C2) =:= abs(R1 - R2),
    ColStep is (C2 - C1) // abs(C2 - C1),
    RowStep is (R2 - R1) // abs(R2 - R1),
    clear_diagonal_step(C1, R1, C2, R2, ColStep, RowStep).


% clear_diagonal_step/6: kiểm tra từng bước trên đường chéo
% hàm này có nhiệm vụ dừng đệ quy khi đến đích hoặc gặp quân cản
clear_diagonal_step(C, R, C2, R2, _, _) :-
    C1 is C + ((C2 - C) // abs(C2 - C)),
    R1 is R + ((R2 - R) // abs(R2 - R)),
    C1 =:= C2, R1 =:= R2, !.
% hàm này sẽ tiếp tục kiểm tra từng bước trên đường chéo 
clear_diagonal_step(C, R, C2, R2, ColStep, RowStep) :-
    C1 is C + ColStep,
    R1 is R + RowStep,
    (C1 =:= C2, R1 =:= R2 -> true
    ;
        (\+ piece_at(C1, R1, _, _)),
        clear_diagonal_step(C1, R1, C2, R2, ColStep, RowStep)
    ).

% between_min_max/4: trả về Min và Max của 2 số
% lấy dải giá trị giữa 2 số A và B để kiểm tra trống trên hàng, cột , chéo 
between_min_max(A, B, Min, Max) :-
    (A < B -> Min is A+1, Max is B-1 ; Min is B+1, Max is A-1).