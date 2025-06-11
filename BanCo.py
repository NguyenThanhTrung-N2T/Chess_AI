import pygame
import os
from pyswip import Prolog

prolog = Prolog()
prolog.consult("Chess_Law.pl")  # Dùng luật cờ viết bằng Prolog

pygame.init()
pygame.mixer.init()

# Âm thanh
game_start_sound = pygame.mixer.Sound("sounds/game_start.MP3")
move_sound = pygame.mixer.Sound("sounds/move.MP3")
castle_sound = pygame.mixer.Sound("sounds/castle.MP3")
capture_sound = pygame.mixer.Sound("sounds/capture.MP3")
check_sound = pygame.mixer.Sound("sounds/check.MP3")
game_over_sound = pygame.mixer.Sound("sounds/game_over.MP3")
game_over_stalemate_sound = pygame.mixer.Sound("sounds/game_over_stalemate.MP3")
click_sound = pygame.mixer.Sound("sounds/click.MP3")

status_msg_font = pygame.font.Font('fonts/pixelmix.ttf', 15)
white_cell_color = (255, 255, 255)
black_cell_color = (127, 164, 209)
selected_square_color = (255, 223, 100)
king_in_check_color = (255, 80, 80)
highlight_square_color = (140, 230, 150)
last_move_square_color = (255, 165, 100)
border_thickness = 5
msg_pos = (726, 500)

class Board:
    def __init__(self, screen, size=80):
        self.screen = screen
        self.size = size
        self.cell_size = size
        self.colors = [white_cell_color, black_cell_color]
        self.images = self.load_images()
        self.in_check_square = None  # Vị trí vua đang bị chiếu
        
        # ➕ Các thuộc tính mới để tự quản lý bàn cờ
        self.board_state = self.init_board_state()  # Trạng thái bàn cờ 8x8
        self.assert_board_state()  # Assert trạng thái ban đầu vào Prolog
        print("Các quân cờ hiện tại từ Prolog:")
        try:
            for sol in prolog.query("piece_at(C, R, Color, Piece)"):
                print(sol)
        except Exception as e:
            print(f"Lỗi khi truy vấn piece_at: {e}")
        self.selected_square = None
        self.move_history = []
        self.turn = "w"
        self.status_message = ""
        self.game_over = False

    def init_board_state(self):
        # w_ = quân trắng, b_ = quân đen, None = ô trống
        return [
            ['b_rook', 'b_knight', 'b_bishop', 'b_queen', 'b_king', 'b_bishop', 'b_knight', 'b_rook'],
            ['b_pawn'] * 8,
            [None] * 8,
            [None] * 8,
            [None] * 8,
            [None] * 8,
            ['w_pawn'] * 8,
            ['w_rook', 'w_knight', 'w_bishop', 'w_queen', 'w_king', 'w_bishop', 'w_knight', 'w_rook']
        ]

    def load_images(self):
        pieces = ['pawn', 'knight', 'bishop', 'rook', 'queen', 'king']
        colors = ['w', 'b']
        images = {}
        for color in colors:
            for piece in pieces:
                filename = f"{color}_{piece}.png"
                path = os.path.join('images', filename)
                if os.path.exists(path):
                    image = pygame.image.load(path)
                    image = pygame.transform.scale(image, (self.size, self.size))
                    images[f"{color}_{piece}"] = image
                else:
                    print(f"Không tìm thấy hình ảnh: {path}")
        return images

    def draw_board(self, offset_x, offset_y):
        for row in range(8):
            for col in range(8):
                color = self.colors[(row + col) % 2]
                rect = pygame.Rect(offset_x + col * self.cell_size, offset_y + row * self.cell_size, self.cell_size, self.cell_size)
                pygame.draw.rect(self.screen, color, rect)

                # Vẽ viền đỏ nếu là vua đang bị chiếu
                if self.in_check_square == (row, col):
                    pygame.draw.rect(self.screen, (255, 0, 0), rect, border_thickness)

                # Vẽ viền màu xanh (hoặc màu khác) nếu là ô đang được chọn
                if self.selected_square == (row, col):
                    pygame.draw.rect(self.screen, selected_square_color, rect, border_thickness)

        # Vẽ thông báo trạng thái
        if self.status_message:
            msg = status_msg_font.render(self.status_message, True, (255, 0, 0))
            self.screen.blit(msg, msg_pos)

    def draw_pieces(self, offset_x, offset_y):
        for row in range(8):
            for col in range(8):
                piece = self.board_state[row][col]
                if piece:
                    image = self.images.get(piece)
                    if image:
                        self.screen.blit(image, (offset_x + col * self.cell_size, offset_y + row * self.cell_size))

    # Phân tích chuỗi quân cờ để lấy loại quân và màu sắc
    def parse_piece(self, piece_str):
        color_code, piece_type = piece_str.split("_")  # e  .g. "w_rook" -> ["w", "rook"]
        color = "white" if color_code == "w" else "black"
        return piece_type, color

    # def highlight_squares(self, offset_x=0, offset_y=0):
    #     if self.selected_square is not None:
    #         row, col = self.selected_square
    #         piece = self.board_state[row][col]
    #         if piece:
    #             piece_type, color = self.parse_piece(piece)
    #             # Chuyển đổi sang tọa độ Prolog
    #             prolog_row = 8 - row
    #             prolog_col = col + 1
    #             # Gọi Prolog lấy các nước đi hợp lệ
    #             query = f"all_legal_moves('{color}', {prolog_col}, {prolog_row}, Moves)"
    #             result = list(prolog.query(query))
    #             moves = []
    #             if result:
    #                 moves = result[0]['Moves']
    #             print("Highlight moves:", moves)
    #             # Vẽ viền các ô hợp lệ
    #             for (c2, r2) in moves:
    #                 py_row = 8 - r2
    #                 py_col = c2 - 1
    #                 rect = pygame.Rect(offset_x + py_col * self.cell_size, offset_y + py_row * self.cell_size, self.cell_size, self.cell_size)
    #                 pygame.draw.rect(self.screen, highlight_square_color, rect, border_thickness)

    # Kiểm tra xem quân cờ có phải là quân của người chơi hiện tại hay không
    def is_player_piece(self, piece):
        # piece: 'w_pawn', 'b_queen', ...
        return piece.startswith(self.turn + "_")

    # Trả về màu sắc hiện tại của người chơi
    def current_color(self):
        return "white" if self.turn == "w" else "black"
    
    # Chuyển đổi lượt chơi
    def switch_turn(self):
        self.turn = "b" if self.turn == "w" else "w"
        self.status_message = f"Turn: {'White' if self.turn == 'w' else 'Black'}"

    def assert_board_state(self):
        # Xóa hết các fact piece_at/4 trong Prolog
        list(prolog.query("retractall(piece_at(_,_,_,_))"))
        # Chỉ assert các ô có quân cờ
        for row in range(8):
            for col in range(8):
                piece = self.board_state[row][col]
                if piece:  # Chỉ khi có quân cờ
                    piece_type, color = self.parse_piece(piece)
                    color_atom = "white" if color == "white" else "black"
                    prolog_row = 8 - row
                    prolog_col = col + 1
                    query = f"assertz(piece_at({prolog_col}, {prolog_row}, {color_atom}, {piece_type}))"
                    list(prolog.query(query))


    def handle_click(self, row_prolog, col_prolog):
        if self.game_over:
            return  # Nếu game kết thúc thì không cho đi tiếp

        row = 8 - row_prolog
        col = col_prolog - 1

        if self.selected_square is None:
            piece = self.board_state[row][col]
            if piece and self.is_player_piece(piece):
                self.selected_square = (row, col)
        else:
            from_row, from_col = self.selected_square
            from_row_prolog = 8 - from_row
            from_col_prolog = from_col + 1
            color = self.current_color()
            piece = self.board_state[from_row][from_col]
            if not piece:
                self.selected_square = None
                return
            piece_type, _ = self.parse_piece(piece)

            self.assert_board_state()

            print("Trạng thái last_move trong Prolog:")
            for sol in prolog.query("last_move(C1, R1, C2, R2)"):
                print(sol)

            query = f"move_piece({piece_type}, {color}, {from_col_prolog}, {from_row_prolog}, {col_prolog}, {row_prolog})"
            result = list(prolog.query(query))
            print(f"Query: {query} -> Result: {result}")

            if result:
                captured_piece = self.board_state[row][col]

                if piece_type == "pawn" and captured_piece is None and from_col != col:
                    direction = 1 if color == "white" else -1
                    captured_row = row - (-direction)
                    self.board_state[captured_row][col] = None

                self.board_state[row][col] = self.board_state[from_row][from_col]
                self.board_state[from_row][from_col] = None

                self.assert_board_state()

                # Kiểm tra các trạng thái đặc biệt sau khi đi quân
                enemy_color = "black" if self.turn == "w" else "white"
                
                # Checkmate
                checkmate_query = f"checkmate({enemy_color})"
                if list(prolog.query(checkmate_query)):
                    self.status_message = f"Checkmate! {color.capitalize()} wins!"
                    game_over_sound.play()
                    self.game_over = True
                    return

                # Stalemate
                stalemate_query = f"stalemate({enemy_color})"
                if list(prolog.query(stalemate_query)):
                    self.status_message = "Stalemate! It's a draw!"
                    game_over_stalemate_sound.play()
                    self.game_over = True
                    return

                # Check
                check_query = f"in_check({enemy_color})"
                print(f"Check query: {check_query}")
                if list(prolog.query(check_query)):
                    self.status_message = f"{enemy_color.capitalize()} is in check!"
                    check_sound.play()

                    # Tìm vị trí quân vua đối thủ
                    for row in range(8):
                        for col in range(8):
                            piece = self.board_state[row][col]
                            if piece and self.is_enemy_king(piece, enemy_color):
                                self.in_check_square = (row, col)
                                break
                else:
                    self.status_message = ""
                    self.in_check_square = None  # Nếu không bị chiếu thì xóa viền

                self.switch_turn()

                if captured_piece or (piece_type == "pawn" and from_col != col):
                    capture_sound.play()
                else:
                    move_sound.play()
            else:
                self.status_message = "Invalid move!"

            self.selected_square = None

    # Kiểm tra xem quân cờ có phải là quân vua của đối thủ hay không
    def is_enemy_king(self, piece, enemy_color):
        piece_type, color = self.parse_piece(piece)
        return piece_type == "king" and color == enemy_color

    # khi cần lưu trạng thái bàn cờ vào Prolog
    # # Sau khi thực hiện nước đi thành công:
    # assert_board_state(self.board_state)  # assert lại trạng thái mới vào Prolog
    # list(prolog.query("save_board_state."))  # lưu vào lịch sử Prolog

    # # Khi cần kiểm tra hòa lặp lại 3 lần:
    # if list(prolog.query("board_repetition(3).")):
    #     self.status_message = "Draw by Threefold Repetition!"




    # # Xử lý sự kiện click chuột ( chọn quân cờ và nước đi )
    # def handle_click(self, row, col):
    #     if self.game_over:
    #         return
        
    #     square = chess.square(col, 7 - row)

    #     if self.selected_square is None:
    #         if self.is_valid_selection(row, col):
    #             self.selected_square = (row, col)
    #             self.status_message = ">> Your king is in check" if self.chess_board.is_check() else ""
    #         else:
    #             self.status_message = ">> Can't select this cell"
    #     else:
    #         from_row, from_col = self.selected_square
    #         from_square = chess.square(from_col, 7 - from_row)
    #         to_square = square

    #         # Nếu vừa ấn quân mình thì không phải nước đi mà chỉ chọn quân khác
    #         selected_piece = self.chess_board.piece_at(from_square)
    #         clicked_piece = self.chess_board.piece_at(to_square)
    #         if clicked_piece and clicked_piece.color == selected_piece.color:
    #             self.selected_square = (row, col)
    #             return
            
    #         # Nước đi
    #         move = chess.Move(from_square, to_square)
    #         piece = self.chess_board.piece_at(from_square)
    #         to_rank = chess.square_rank(to_square)

    #         # Tạm thời dùng hậu làm quân phong mặc định để kiểm tra hợp lệ
    #         temp_move = chess.Move(from_square, to_square, promotion=chess.QUEEN)

    #         if piece.piece_type == chess.PAWN and to_rank in [0, 7] and temp_move in self.chess_board.legal_moves:
    #             # Nếu đi được và là phong cấp → cho người chơi chọn quân
    #             promotion_piece = self.show_promotion_menu()
    #             move = chess.Move(from_square, to_square, promotion=promotion_piece)
    #         else:
    #             move = chess.Move(from_square, to_square)

    #         if move in self.chess_board.legal_moves:
    #             is_capture = False
    #             is_castling = False
    #             current_move = chess.Move.from_uci(move.uci())
    #             if self.chess_board.is_capture(current_move):
    #                 is_capture = True
    #             elif self.chess_board.is_castling(current_move):
    #                 is_castling = True
    #             self.move_history.append(move.uci())
    #             self.chess_board.push(move)
    #             if self.chess_board.is_check():
    #                 king_sq = self.chess_board.king(self.chess_board.turn)
    #                 row_check = 7 - chess.square_rank(king_sq)
    #                 col_check = chess.square_file(king_sq)
    #                 self.king_in_check_square = (row_check, col_check)
    #                 self.status_message = ">> Your king is in check "
    #                 # Check SOUND
    #                 check_sound.play()
    #             else:
    #                 self.king_in_check_square = None
    #                 self.status_message = ""
    #                 # Move and capture SOUND    
    #                 if is_capture:
    #                     capture_sound.play()
    #                 elif is_castling:
    #                     castle_sound.play()
    #                 else:
    #                     move_sound.play()

    #             if self.check_game_end():
    #                 self.game_over = True
    #                 # Game over SOUND
    #                 if self.status_message.find("Stalemate") != -1:
    #                     game_over_stalemate_sound.play()
    #                 else:
    #                     game_over_sound.play()
    #                 return

    #             # Gọi AI nếu có
    #             if self.play_with_ai and self.chess_board.turn == self.ai_player:
    #                 ai_move = self.ai.select_move(self.chess_board)

    #                 if ai_move and ai_move in self.chess_board.legal_moves:
    #                     move_san = self.chess_board.san(ai_move)  # Gọi san() trước khi push()
    #                     self.chess_board.push(ai_move)
    #                     self.move_history.append(ai_move.uci())

    #                     if self.chess_board.is_check():
    #                         king_sq = self.chess_board.king(self.chess_board.turn)
    #                         row_check = 7 - chess.square_rank(king_sq)
    #                         col_check = chess.square_file(king_sq)
    #                         self.king_in_check_square = (row_check, col_check)
    #                         self.status_message = ">> Your king is in check "
    #                     else:
    #                         self.king_in_check_square = None
    #                         self.status_message = ""

    #                     if self.check_game_end():
    #                         self.game_over = True
    #                 else:
    #                     print(f"[LỖI] AI trả về nước đi không hợp lệ: {ai_move} - {self.chess_board.fen()}")

    #         else:
    #             self.status_message = ">> Invalid move "

    #         self.selected_square = None

    # # kiểm tra xem ô được chọn có phải là quân của người chơi hay không
    # def is_valid_selection(self, row, col):
    #     square = chess.square(col, 7 - row)
    #     piece = self.chess_board.piece_at(square)
    #     return piece is not None and piece.color == self.chess_board.turn

    # # Kiểm tra kết thúc trò chơi
    # def check_game_end(self):
    #     if self.chess_board.is_checkmate():
    #         winner = "White" if not self.chess_board.turn else "Black"
    #         self.status_message = f"{winner} wins !"
    #         return True
    #     elif self.chess_board.is_stalemate():
    #         self.status_message = "Draw by Stalemate !"
    #         return True
    #     elif self.chess_board.is_insufficient_material():
    #         self.status_message = "Draw by Insufficient Material !"
    #         return True
    #     elif self.chess_board.can_claim_fifty_moves():
    #         self.status_message = "Draw by 50 Moves !"
    #         return True
    #     elif self.chess_board.can_claim_threefold_repetition():
    #         self.status_message = "Draw by Threefold Repetiton !"
    #         return True
    #     return False

    # # Reset game state
    # def reset_game(self):
    #     self.chess_board = chess.Board()
    #     self.move_history = []
    #     self.selected_square = None
    #     self.king_in_check_square = None
    #     self.status_message = ""
    #     self.game_over = False
    #     # Game start sound
    #     game_start_sound.play()

    # # Hoàn tác nước đi
    # def undo_move(self):
    #     if self.move_history:
    #         self.move_history.pop()
    #         self.chess_board.pop()
    #         self.selected_square = None

    #         if self.chess_board.is_check():
    #             king_sq = self.chess_board.king(self.chess_board.turn)
    #             row_check = 7 - chess.square_rank(king_sq)
    #             col_check = chess.square_file(king_sq)
    #             self.king_in_check_square = (row_check, col_check)
    #             self.status_message = ">> Your king is in check!"
    #         else:
    #             self.king_in_check_square = None
    #             self.status_message = ""
    #     # Click SOUND
    #     click_sound.play()
    #     self.game_over = False

    # # Hiển thị menu phong cấp khi người chơi đi quân tốt đến hàng cuối
    # def show_promotion_menu(self):
    #     pygame.font.init()
        
    #     menu_color =  (230, 220, 255)
    #     menu_border_color = (100, 80, 160)
    #     menu_btn_color = (140, 120, 200)
    #     icon_size = (70, 70)
    #     text_color = (30, 60, 30)
    #     start_x = 270
    #     start_y = 270
    #     offset = 5
    #     menu_size = 160
    #     menu_size1 = 40
    #     border_size = 6
    #     text_size = 19
    #     btn_size = 70
    #     border_thickness = 6
    #     gap = 10  

    #     menu_rect = pygame.Rect(start_x, start_y, menu_size, menu_size)
    #     border_rect = pygame.Rect(0, 0, menu_size + border_thickness, menu_size + border_thickness)
    #     border_rect.center = menu_rect.center

    #     menu_rect1 = pygame.Rect(0, 0, menu_size,  menu_size1)
    #     menu_rect1.bottomleft = (menu_rect.left, menu_rect.top - border_thickness - 1)
    #     border_rect1 = pygame.Rect(0, 0, menu_size + border_thickness, menu_size1 + border_thickness)
    #     border_rect1.center = menu_rect1.center

    #     font = pygame.font.Font("fonts/pixelmix_bold.ttf", text_size)
    #     text = font.render("PROMOTION", False, text_color)
    #     text_rect = text.get_rect(center = menu_rect1.center)
    #     overlay = pygame.Surface((menu_size, menu_size), pygame.SRCALPHA)
    #     overlay.set_alpha(230)
    #     overlay.fill(menu_color)
        
    #     btn_queen  = pygame.Rect(offset + start_x, offset + start_y , btn_size, btn_size)
    #     btn_bishop = pygame.Rect(offset + start_x + gap + btn_size, offset + start_y ,btn_size, btn_size)
    #     btn_knight = pygame.Rect(offset + start_x, offset + start_y + gap + btn_size, btn_size, btn_size)
    #     btn_rook   = pygame.Rect(offset + start_x + gap + btn_size, offset + start_y + gap + btn_size, btn_size, btn_size)

    #     # Load và resize icon
    #     color = 'w' if self.chess_board.turn == chess.WHITE else 'b'
    #     queen_icon  = pygame.image.load(f"images/{color}_queen.png")
    #     rook_icon   = pygame.image.load(f"images/{color}_rook.png")
    #     bishop_icon = pygame.image.load(f"images/{color}_bishop.png")
    #     knight_icon = pygame.image.load(f"images/{color}_knight.png")

    #     queen_icon = pygame.transform.scale(queen_icon, icon_size)
    #     rook_icon = pygame.transform.scale(rook_icon, icon_size)
    #     bishop_icon = pygame.transform.scale(bishop_icon, icon_size)
    #     knight_icon = pygame.transform.scale(knight_icon, icon_size)

    #     while True:
    #         mouse_pos = pygame.mouse.get_pos()

    #         pygame.draw.rect(self.screen, menu_border_color, border_rect, width=border_size)
    #         pygame.draw.rect(self.screen, menu_border_color, border_rect1, width=border_size)
    #         self.screen.blit(overlay, menu_rect.topleft)
    #         pygame.draw.rect(self.screen, menu_color, menu_rect1)    
    #         self.screen.blit(text, text_rect)
            
    #         def draw_button(btn, icon):
    #             is_hovered = btn.collidepoint(mouse_pos)
    #             color = tuple(c + 20 for c in menu_btn_color) if is_hovered else menu_btn_color
    #             pygame.draw.rect(self.screen, color, btn)
    #             self.screen.blit(icon, (btn.left, btn.top - offset))
    #         draw_button(btn_queen, queen_icon)
    #         draw_button(btn_bishop, bishop_icon)
    #         draw_button(btn_knight, knight_icon)
    #         draw_button(btn_rook, rook_icon)

    #         pygame.display.flip()
    #         for event in pygame.event.get():
    #             if event.type == pygame.QUIT:
    #                 pygame.quit()
    #                 return None
    #             if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
    #                 mouse_pos = event.pos
    #                 if btn_queen.collidepoint(mouse_pos):
    #                     return chess.QUEEN
    #                 elif btn_rook.collidepoint(mouse_pos):
    #                     return chess.ROOK
    #                 elif btn_bishop.collidepoint(mouse_pos):
    #                     return chess.BISHOP
    #                 elif btn_knight.collidepoint(mouse_pos):
    #                     return chess.KNIGHT
