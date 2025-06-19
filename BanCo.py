import pygame
import os

from pyswip import Prolog
# prolog instance sẽ được truyền từ Main.py
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
    def __init__(self, screen, size=80, prolog_engine=None):
        self.screen = screen
        self.size = size
        self.cell_size = size
        self.colors = [white_cell_color, black_cell_color]
        self.images = self.load_images()
        self.in_check_square = None  # Vị trí vua đang bị chiếu

        if prolog_engine is None:
            # Dự phòng nếu không được cung cấp, mặc dù nó luôn phải được cung cấp từ Main
            print("CẢNH BÁO: prolog_engine không được cung cấp cho Board, tạo instance mới.")
            self.prolog_engine = Prolog()
            self.prolog_engine.consult("Chess_Helper.pl")
            self.prolog_engine.consult("Chess_Law.pl")
            self.prolog_engine.consult("Chess_AI.pl")
        else:
            self.prolog_engine = prolog_engine

        # ➕ Các thuộc tính mới để tự quản lý bàn cờ
        self.board_state = self.init_board_state()  # Trạng thái bàn cờ 8x8
        self.assert_board_state()  # Assert trạng thái ban đầu vào Prolog
        print("Các quân cờ hiện tại từ Prolog:")
        try:
            for sol in self.prolog_engine.query("piece_at(C, R, Color, Piece)"): # Sửa ở đây
                print(sol)
        except Exception as e:
            print(f"Lỗi khi truy vấn piece_at: {e}")
        self.selected_square = None
        self.move_history = []
        self.turn = "w"
        self.status_message = ""
        self.game_over = False
        self.play_with_ai = False # Sẽ được đặt từ Main.py
        self.ai = None # Sẽ được đặt từ Main.py
        self.ai_level = 1 # Mặc định
        self.ai_color_char = 'b' # Mặc định AI chơi quân đen (có thể thay đổi nếu muốn AI chơi Trắng)

    def reset_prolog_dynamic_facts(self):
        """Reset các fact động trong Prolog về trạng thái ban đầu."""
        # Sử dụng self.prolog_engine
        list(self.prolog_engine.query("retractall(last_move(_,_,_,_)), assertz(last_move(0,0,0,0))"))
        list(self.prolog_engine.query("retractall(king_moved(_))"))
        list(self.prolog_engine.query("retractall(rook_moved(_,_))"))
        list(self.prolog_engine.query("retractall(halfmove_clock(_)), assertz(halfmove_clock(0))"))
        list(self.prolog_engine.query("retractall(board_history(_)), assertz(board_history([]))"))
        print("Dynamic Prolog facts (last_move, king_moved, etc.) reset by Board's reset_prolog_dynamic_facts.")

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

    # Tải hình ảnh quân cờ từ thư mục 'images'
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

    # Vẽ bàn cờ
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

    # Vẽ các quân cờ trên bàn cờ
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
    

    # Vẽ các ô hợp lệ cho quân cờ đã chọn ( đang fix )
    # def highlight_squares(self, offset_x=0, offset_y=0):
    #     if self.selected_square is None:
    #         return  # Chưa chọn quân thì không highlight

    #     from_row, from_col = self.selected_square
    #     piece = self.board_state[from_row][from_col]

    #     if not piece:
    #         return  # Tránh lỗi nếu ô được chọn không có quân

    #     piece_type, color = self.parse_piece(piece)

    #     # Tọa độ Prolog
    #     from_row_prolog = 8 - from_row
    #     from_col_prolog = from_col + 1

    #     # Duyệt toàn bộ bàn cờ
    #     for to_row in range(8):
    #         for to_col in range(8):
    #             to_row_prolog = 8 - to_row
    #             to_col_prolog = to_col + 1

    #             # Bỏ qua nếu là chính ô đang đứng
    #             if from_row == to_row and from_col == to_col:
    #                 continue

    #             # Gọi Prolog để kiểm tra xem có đi hợp lệ không
    #             query = f"move_piece({piece_type}, {color}, {from_col_prolog}, {from_row_prolog}, {to_col_prolog}, {to_row_prolog})"
    #             result = list(prolog.query(query))

    #             if result:
    #                 # Nếu hợp lệ thì vẽ viền
    #                 rect = pygame.Rect(offset_x + to_col * self.cell_size, offset_y + to_row * self.cell_size, self.cell_size, self.cell_size)
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

    # Phương thức assert_board_state để cập nhật trạng thái bàn cờ vào Prolog
    def assert_board_state(self):
        # Xóa hết các fact piece_at/4 trong Prolog
        list(self.prolog_engine.query("retractall(piece_at(_,_,_,_))"))
        # Chỉ assert các ô có quân cờ
        for row in range(8):
            for col in range(8):
                piece = self.board_state[row][col]
                if piece:  # Chỉ khi có quân cờ
                    piece_type, color = self.parse_piece(piece)
                    color_atom = color # parse_piece đã trả về "white" hoặc "black"
                    prolog_row = 8 - row
                    prolog_col = col + 1
                    query = f"assertz(piece_at({prolog_col}, {prolog_row}, {color_atom}, {piece_type}))." # Thêm dấu chấm cuối query
                    try:
                        list(self.prolog_engine.query(query)) # Sửa ở đây
                    except Exception as e:
                        print(f"Lỗi khi assert piece_at: {query} - {e}")


    def handle_click(self, row_prolog, col_prolog):
        # row_prolog, col_prolog là tọa độ Prolog của ô được click
        if self.game_over:
            return

        # Nếu là lượt AI và đang chơi với AI, không cho người dùng click
        if self.play_with_ai and self.turn == self.ai_color_char:
            print("AI is thinking... Please wait.")
            return

        # Xử lý logic di chuyển của người chơi
        self._process_move_attempt(row_prolog, col_prolog, is_ai_move=False)

        # Sau khi người chơi đi, nếu không game over và là lượt AI
        if not self.game_over and self.play_with_ai and self.turn == self.ai_color_char:
            pygame.display.flip() # Cập nhật màn hình để hiển thị nước đi của người
            pygame.time.wait(100) # Đợi một chút để người chơi thấy nước đi của mình
            self.ai_perform_move()

    def ai_perform_move(self):
        if not self.play_with_ai or self.turn != self.ai_color_char or self.game_over:
            return

        ai_prolog_color = self.current_color() # "white" hoặc "black"
        print(f"AI ({ai_prolog_color}, level {self.ai_level}) is thinking...")
        
        # Đảm bảo Prolog có trạng thái bàn cờ mới nhất trước khi AI quyết định
        # self.assert_board_state() # Quan trọng, nhưng _process_move_attempt cũng sẽ gọi

        ai_move_tuple = self.ai.select_move(ai_prolog_color)

        if ai_move_tuple:
            piece_type_str, c1, r1, c2, r2 = ai_move_tuple
            print(f"AI intends to move: {piece_type_str} from ({c1},{r1}) to ({c2},{r2}) for color {ai_prolog_color}")
            
            # AI đã chọn nước đi, giờ xử lý nó
            self._process_move_attempt(r2, c2, # to_row_prolog, to_col_prolog
                                       is_ai_move=True, 
                                       ai_piece_type=piece_type_str, 
                                       ai_from_col=c1, 
                                       ai_from_row=r1)
        else:
            # AI không tìm thấy nước đi -> game có thể đã kết thúc (Prolog đã xử lý chiếu bí/hết nước cho AI)
            print("AI has no moves. Checking game status for AI.")
            self.check_game_status_after_move(ai_prolog_color) # Kiểm tra cho chính AI

    def _process_move_attempt(self, to_row_prolog, to_col_prolog, is_ai_move=False, ai_piece_type=None, ai_from_col=None, ai_from_row=None):
        """
        Xử lý một nỗ lực di chuyển, từ người chơi hoặc AI.
        Đối với người chơi: to_row_prolog, to_col_prolog là ô đích được click.
        Đối với AI: ai_piece_type, ai_from_col, ai_from_row là ô xuất phát của nước đi đã chọn,
                     và to_row_prolog, to_col_prolog là ô đích của nước đi đã chọn.
        """
        to_row_py = 8 - to_row_prolog
        to_col_py = to_col_prolog - 1

        current_player_color_prolog = self.current_color() # "white" hoặc "black"

        if is_ai_move:
            from_row_prolog_move = ai_from_row
            from_col_prolog_move = ai_from_col
            piece_type_move = ai_piece_type
        else: # Nước đi của người
            if self.selected_square is None:
                # Người chơi chọn một quân cờ
                piece_on_square = self.board_state[to_row_py][to_col_py] # to_row_py, to_col_py là ô vừa click
                if piece_on_square and self.is_player_piece(piece_on_square):
                    self.selected_square = (to_row_py, to_col_py)
                return # Đợi click thứ hai (ô đích)
            else:
                # Người chơi đã chọn quân và giờ chọn ô đích
                from_row_py_selected, from_col_py_selected = self.selected_square
                from_row_prolog_move = 8 - from_row_py_selected
                from_col_prolog_move = from_col_py_selected + 1
                
                piece_at_selected = self.board_state[from_row_py_selected][from_col_py_selected]
                if not piece_at_selected: 
                    self.selected_square = None
                    return
                piece_type_move, _ = self.parse_piece(piece_at_selected)

                if (to_row_py, to_col_py) == (from_row_py_selected, from_col_py_selected): # Click lại quân cũ
                    self.selected_square = None
                    return
                
                clicked_piece_on_board = self.board_state[to_row_py][to_col_py] # Ô đích vừa click
                if clicked_piece_on_board and self.is_player_piece(clicked_piece_on_board): # Click vào quân khác cùng màu
                    self.selected_square = (to_row_py, to_col_py) # Đổi quân chọn
                    return

        # Đảm bảo Prolog có trạng thái bàn cờ hiện tại trước khi thử move_piece
        self.assert_board_state() 

        # Gọi move_piece trong Prolog
        query = f"move_piece({piece_type_move}, {current_player_color_prolog}, {from_col_prolog_move}, {from_row_prolog_move}, {to_col_prolog}, {to_row_prolog})."
        result = list(self.prolog_engine.query(query))
        print(f"Prolog move_piece query: {query} -> Result: {len(result)>0}")

        if result: # move_piece thành công trong Prolog
            # Cập nhật board_state của Python dựa trên nước đi thành công của Prolog
            py_from_row, py_from_col = 8 - from_row_prolog_move, from_col_prolog_move - 1
            
            piece_to_move_on_py_board = self.board_state[py_from_row][py_from_col]
            
            # Di chuyển chuẩn trên bàn cờ Python
            captured_piece_on_py_board = self.board_state[to_row_py][to_col_py] # Quân ở ô đích (nếu có)
            self.board_state[to_row_py][to_col_py] = piece_to_move_on_py_board
            self.board_state[py_from_row][py_from_col] = None

            # Xử lý cập nhật đặc biệt cho board_state Python dựa trên những gì move_piece đã làm
            # Bắt tốt qua đường (En Passant):
            if piece_type_move == "pawn" and captured_piece_on_py_board is None and abs(to_col_prolog - from_col_prolog_move) == 1:
                # Prolog's move_piece đã xóa quân tốt bị bắt. Python cần làm tương tự.
                pawn_dir_val = 1 if current_player_color_prolog == "white" else -1 # Hướng quân tốt của đối phương di chuyển
                en_passant_captured_pawn_row_py = to_row_py + pawn_dir_val # Hàng của quân tốt bị bắt (theo Python)
                if 0 <= en_passant_captured_pawn_row_py < 8:
                    self.board_state[en_passant_captured_pawn_row_py][to_col_py] = None
                    print(f"Python board: En passant capture visual update at ({en_passant_captured_pawn_row_py}, {to_col_py})")

            # Nhập thành (Castling):
            if piece_type_move == "king" and abs(to_col_prolog - from_col_prolog_move) == 2:
                rook_orig_col_py, rook_dest_col_py = -1, -1
                if to_col_prolog > from_col_prolog_move: # Nhập thành gần (vua sang G)
                    rook_orig_col_py, rook_dest_col_py = 7, 5 # Xe từ H sang F (theo Python)
                else: # Nhập thành xa (vua sang C)
                    rook_orig_col_py, rook_dest_col_py = 0, 3 # Xe từ A sang D (theo Python)
                
                self.board_state[to_row_py][rook_dest_col_py] = self.board_state[to_row_py][rook_orig_col_py]
                self.board_state[to_row_py][rook_orig_col_py] = None
                castle_sound.play()
            elif captured_piece_on_py_board or \
                 (piece_type_move == "pawn" and abs(to_col_prolog - from_col_prolog_move) == 1): # Nếu có bắt quân (thường hoặc en passant)
                capture_sound.play()
            else: # Nước đi thường
                move_sound.play()

            # Phong cấp (Pawn Promotion)
            if piece_type_move == "pawn" and (to_row_prolog == 8 or to_row_prolog == 1): # Đến hàng cuối
                promoted_to_piece_type = "queen" # AI mặc định phong Hậu
                if not is_ai_move: # Người chơi
                    promoted_to_piece_type_user = self.show_promotion_menu()
                    if promoted_to_piece_type_user: # Nếu người dùng chọn
                        promoted_to_piece_type = promoted_to_piece_type_user
                    # Nếu người dùng hủy, promoted_to_piece_type vẫn là 'queen' (hoặc bạn có thể xử lý khác)

                # Cập nhật Prolog
                promo_query = f"promote_pawn({to_col_prolog}, {to_row_prolog}, {current_player_color_prolog}, {promoted_to_piece_type})."
                list(self.prolog_engine.query(promo_query))
                print(f"Prolog promotion query: {promo_query}")
                # Cập nhật board_state Python
                self.board_state[to_row_py][to_col_py] = f"{current_player_color_prolog[0]}_{promoted_to_piece_type}"

            if not is_ai_move: # Nếu là người chơi
                self.selected_square = None # Bỏ chọn quân

            # Sau khi di chuyển thành công và cập nhật, kiểm tra trạng thái game cho *đối thủ*
            opponent_color_prolog = "black" if current_player_color_prolog == "white" else "white"
            self.check_game_status_after_move(opponent_color_prolog)

            if not self.game_over:
                self.switch_turn()
        
        else: # move_piece thất bại trong Prolog
            if not is_ai_move: # Chỉ hiển thị "Invalid move!" cho người chơi
                self.status_message = "Invalid move!"
            self.selected_square = None # Bỏ chọn dù nước đi thất bại

    def check_game_status_after_move(self, color_to_check_for_prolog):
        """Kiểm tra trạng thái game (chiếu, chiếu bí, hết nước,...) cho màu được chỉ định."""
        # Đảm bảo Prolog có trạng thái mới nhất từ board_state Python trước khi kiểm tra
        self.assert_board_state()

        # Chiếu bí (Checkmate)
        if list(self.prolog_engine.query(f"checkmate({color_to_check_for_prolog})")):
            winner_color_char = 'w' if color_to_check_for_prolog == "black" else 'b' # Người thắng là người vừa đi
            winner_display = "White" if winner_color_char == 'w' else "Black"
            self.status_message = f"Checkmate! {winner_display} wins!"
            game_over_sound.play()
            self.game_over = True
            return

        # Hết nước đi (Stalemate)
        if list(self.prolog_engine.query(f"stalemate({color_to_check_for_prolog})")):
            self.status_message = "Stalemate! It's a draw!"
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do luật 50 nước
        if list(self.prolog_engine.query("draw_by_fifty_moves")):
            self.status_message = "Draw by 50-move rule!"
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do lặp lại 3 lần
        if list(self.prolog_engine.query("draw_by_threefold_repetition")):
            self.status_message = "Draw by threefold repetition!"
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do không đủ quân chiếu bí
        if list(self.prolog_engine.query("insufficient_material")):
            self.status_message = "Draw by insufficient material!"
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Bị chiếu (Check)
        if list(self.prolog_engine.query(f"in_check({color_to_check_for_prolog})")):
            self.status_message = f"{color_to_check_for_prolog.capitalize()} is in check!"
            check_sound.play()
            # Tìm vua của 'color_to_check_for_prolog' để tô màu
            king_pos_query = f"piece_at(KC, KR, {color_to_check_for_prolog}, king)."
            king_sol = list(self.prolog_engine.query(king_pos_query))
            if king_sol:
                kr_prolog, kc_prolog = king_sol[0]['KR'], king_sol[0]['KC']
                self.in_check_square = (8 - kr_prolog, kc_prolog - 1) # Tọa độ Python
        else:
            # Chỉ xóa status_message nếu không phải là thông báo kết thúc game
            if not self.game_over:
                 self.status_message = f"Turn: {'White' if self.turn == 'w' else 'Black'}" # Thông báo lượt mặc định
            self.in_check_square = None

    # Kiểm tra xem quân cờ có phải là quân vua của đối thủ hay không
    def is_enemy_king(self, piece, enemy_color):
        piece_type, color = self.parse_piece(piece)
        return piece_type == "king" and color == enemy_color

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

    # Reset game state
    def reset_game(self):
        self.board_state = self.init_board_state() # Reset trạng thái bàn cờ Python
        self.reset_prolog_dynamic_facts() # Reset các fact động của Prolog
        self.assert_board_state() # Cập nhật Prolog với trạng thái bàn cờ mới

        self.turn = "w" # Lượt đi đầu tiên luôn là Trắng
        self.selected_square = None
        self.in_check_square = None # Xóa trạng thái vua bị chiếu
        self.status_message = "Turn: White" # Thông báo lượt đi ban đầu
        self.game_over = False
        self.move_history = [] # Xóa lịch sử nước đi
        # Không thay đổi self.play_with_ai, self.ai, self.ai_level, self.ai_color_char
        game_start_sound.play() # Phát âm thanh bắt đầu game mới

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
    def show_promotion_menu(self):
        pygame.font.init()
        
        menu_color =  (230, 220, 255)
        menu_border_color = (100, 80, 160)
        menu_btn_color = (140, 120, 200)
        icon_size = (70, 70)
        text_color = (30, 60, 30)
        start_x = 270
        start_y = 270
        offset = 5
        menu_size = 160
        menu_size1 = 40
        border_size = 6
        text_size = 19
        btn_size = 70
        border_thickness = 6
        gap = 10  

        menu_rect = pygame.Rect(start_x, start_y, menu_size, menu_size)
        border_rect = pygame.Rect(0, 0, menu_size + border_thickness, menu_size + border_thickness)
        border_rect.center = menu_rect.center

        menu_rect1 = pygame.Rect(0, 0, menu_size,  menu_size1)
        menu_rect1.bottomleft = (menu_rect.left, menu_rect.top - border_thickness - 1)
        border_rect1 = pygame.Rect(0, 0, menu_size + border_thickness, menu_size1 + border_thickness)
        border_rect1.center = menu_rect1.center

        font = pygame.font.Font("fonts/pixelmix_bold.ttf", text_size)
        text = font.render("PROMOTION", False, text_color)
        text_rect = text.get_rect(center = menu_rect1.center)
        overlay = pygame.Surface((menu_size, menu_size), pygame.SRCALPHA)
        overlay.set_alpha(230)
        overlay.fill(menu_color)
        
        btn_queen  = pygame.Rect(offset + start_x, offset + start_y , btn_size, btn_size)
        btn_bishop = pygame.Rect(offset + start_x + gap + btn_size, offset + start_y ,btn_size, btn_size)
        btn_knight = pygame.Rect(offset + start_x, offset + start_y + gap + btn_size, btn_size, btn_size)
        btn_rook   = pygame.Rect(offset + start_x + gap + btn_size, offset + start_y + gap + btn_size, btn_size, btn_size)

        # Load và resize icon
        color = 'w' if self.turn == 'w' else 'b'
        queen_icon  = pygame.image.load(f"images/{color}_queen.png")
        rook_icon   = pygame.image.load(f"images/{color}_rook.png")
        bishop_icon = pygame.image.load(f"images/{color}_bishop.png")
        knight_icon = pygame.image.load(f"images/{color}_knight.png")

        queen_icon = pygame.transform.scale(queen_icon, icon_size)
        rook_icon = pygame.transform.scale(rook_icon, icon_size)
        bishop_icon = pygame.transform.scale(bishop_icon, icon_size)
        knight_icon = pygame.transform.scale(knight_icon, icon_size)

        while True:
            mouse_pos = pygame.mouse.get_pos()

            pygame.draw.rect(self.screen, menu_border_color, border_rect, width=border_size)
            pygame.draw.rect(self.screen, menu_border_color, border_rect1, width=border_size)
            self.screen.blit(overlay, menu_rect.topleft)
            pygame.draw.rect(self.screen, menu_color, menu_rect1)    
            self.screen.blit(text, text_rect)
            
            def draw_button(btn, icon):
                is_hovered = btn.collidepoint(mouse_pos)
                color = tuple(c + 20 for c in menu_btn_color) if is_hovered else menu_btn_color
                pygame.draw.rect(self.screen, color, btn)
                self.screen.blit(icon, (btn.left, btn.top - offset))
            draw_button(btn_queen, queen_icon)
            draw_button(btn_bishop, bishop_icon)
            draw_button(btn_knight, knight_icon)
            draw_button(btn_rook, rook_icon)

            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    return None
                if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                    mouse_pos = event.pos
                    if btn_queen.collidepoint(mouse_pos):
                        return "queen"
                    elif btn_rook.collidepoint(mouse_pos):
                        return "rook"
                    elif btn_bishop.collidepoint(mouse_pos):
                        return "bishop"
                    elif btn_knight.collidepoint(mouse_pos):
                        return "knight"