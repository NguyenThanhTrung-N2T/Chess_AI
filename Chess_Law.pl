% Chess_Law.pl

% --- Khai báo dynamic ---
:- consult('Chess_Helper.pl').
:- dynamic piece_at/4.
:- dynamic last_move/4.
% đánh dấu khi vua và xe của màu đó đã di chuyển
:- dynamic king_moved/1.
:- dynamic rook_moved/2.
% đếm nửa nước (halfmove clock) để tính nước đi ( hòa 50 nước đi không ăn quân)
:- dynamic halfmove_clock/1.
halfmove_clock(0). % Đếm nửa nước (mỗi lần di chuyển là +1)

last_move(0,0,0,0). % Fact để lưu nước đi cuối cùng dùng trong en_passant

% Xác định màu quân đối thủ
opponent_color(white, black).
opponent_color(black, white).


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
    bishop_move_path(C1, R1, C2, R2);
    rook_move_path(C1, R1, C2, R2).

% --- King ---
king_move(C1, R1, C2, R2) :-
    abs(C1 - C2) =< 1, abs(R1 - R2) =< 1,
    (C1 \= C2; R1 \= R2),
    C2 >= 1, C2 =< 8, R2 >= 1, R2 =< 8,
    (\+ piece_at(C2, R2, _, _) ; (piece_at(C2, R2, Color2, _), piece_at(C1, R1, Color1, king), Color1 \= Color2)).

% --- Luật tổng quát kiểm tra nước đi hợp lệ ---
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

% --- Kiểm tra quân vua bị chiếu --- ( chưa fix xong )
in_check(Color) :-
    opponent_color(Color, OpponentColor),
    piece_at(KingC, KingR, Color, king),
    piece_at(C, R, OpponentColor, Piece),
    is_attacking(Piece, OpponentColor, C, R, KingC, KingR),
    !.
% --- Kiểm tra chiếu hết --- ( chưa fix xong )
checkmate(Color) :-
    in_check(Color),
    \+ (piece_at(C, R, Color, Piece),
        piece_at(CK, RK, Color, king),
        between(1, 8, NewC),
        between(1, 8, NewR),
        legal_move(Piece, Color, C, R, NewC, NewR),
        (\+ causes_check(Piece, Color, C, R, NewC, NewR))
    ).
% Hết nước đi: Vua không bị chiếu nhưng không còn nước đi hợp lệ
stalemate(Color) :-
    \+ in_check(Color),
    \+ has_legal_move(Color).

% Kiểm tra hòa do 50 nước đi không ăn quân
draw_by_fifty_moves :-
    halfmove_clock(N),
    N >= 100. % 100 nửa nước = 50 nước đầy đủ

% Kiểm tra còn nước đi hợp lệ không
has_legal_move(Color) :-
    piece_at(C1, R1, Color, Piece),
    between(1, 8, C2),
    between(1, 8, R2),
    legal_move(Piece, Color, C1, R1, C2, R2),
    \+ causes_check(Piece, Color, C1, R1, C2, R2),
    !. % Tìm thấy ít nhất 1 nước đi hợp lệ thì dừng

% Kiểm tra nếu di chuyển quân này có làm cho vua bị chiếu không
causes_check(Piece, Color, C1, R1, C2, R2) :-
    % Lưu trạng thái last_move hiện tại
    last_move(LMC1, LMR1, LMC2, LMR2),
    
    % Tạm thời di chuyển quân cờ
    retract(piece_at(C1, R1, Color, Piece)),
    (retract(piece_at(C2, R2, _, CapturedPiece)) ; CapturedPiece = none),
    assertz(piece_at(C2, R2, Color, Piece)),
    
    % Kiểm tra vua có bị chiếu không
    (in_check(Color) -> Result = true ; Result = false),
    
    % Hoàn tác nước đi
    retract(piece_at(C2, R2, Color, Piece)),
    (CapturedPiece \= none -> assertz(piece_at(C2, R2, _, CapturedPiece)) ; true),
    assertz(piece_at(C1, R1, Color, Piece)),
    
    % Hoàn tác last_move
    retractall(last_move(_, _, _, _)),
    assertz(last_move(LMC1, LMR1, LMC2, LMR2)),
    
    Result = true.

% --- Hàm di chuyển quân cờ tổng quát ---
move_piece(Piece, Color, C1, R1, C2, R2) :-
    legal_move(Piece, Color, C1, R1, C2, R2),

    % Kiểm tra có quân bị ăn không
    (piece_at(C2, R2, _, _) -> Captured = true ; Captured = false),

    % Thực hiện di chuyển
    retract(piece_at(C1, R1, Color, Piece)),

    (   % Nếu là en passant
        (Piece = pawn, en_passant(Color, C1, R1, C2, R2))
    ->  % Xóa tốt bị bắt qua đường
        pawn_dir(Color, Dir, _),
        RowPawn is R2 - Dir,
        retract(piece_at(C2, RowPawn, _, pawn)),
        CapturedFlag = true
    ;   % Nếu không phải en passant
        (Captured -> retract(piece_at(C2, R2, _, _)) ; true),
        CapturedFlag = Captured
    ),

    assertz(piece_at(C2, R2, Color, Piece)),

    (\+ in_check(Color)),

    retractall(last_move(_,_,_,_)),
    assertz(last_move(C1, R1, C2, R2)),

    % Đánh dấu nếu quân xe di chuyển
    (Piece = rook ->
        (retractall(rook_moved(Color, C1)), assertz(rook_moved(Color, C1)))
    ; true),

    % Đánh dấu nếu quân vua di chuyển
    (Piece = king -> 
        (retractall(king_moved(Color)), assertz(king_moved(Color))),
        (abs(C2 - C1) =:= 2 ->  % Nếu nhập thành
            (C2 > C1 ->
                RookColOld is 8, RookColNew is 6
            ;
                RookColOld is 1, RookColNew is 4
            ),
            retract(piece_at(RookColOld, R1, Color, rook)),
            assertz(piece_at(RookColNew, R1, Color, rook))
        ; true)
    ; true),

    % Cập nhật đồng hồ 50 nước
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

    !.
