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

        # Các thuộc tính mới để tự quản lý bàn cờ
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
        self.turn = "w" # 'w' for white, 'b' for black
        self.status_message = ""
        self.game_over = False
        self.highlighted_squares_prolog_coords = [] # Lưu các ô (C2,R2) Prolog cần highlight
        self.play_with_ai = False # Sẽ được đặt từ Main.py
        self.ai = None # Sẽ được đặt từ Main.py
        self.ai_level = 1 # Mặc định
        self.ai_color_char = 'b' # Mặc định AI chơi quân đen (có thể thay đổi nếu muốn AI chơi Trắng)

    # Helper methods to get Prolog dynamic facts
    def get_prolog_fact(self, query_template, vars_to_extract):
        try:
            solutions = list(self.prolog_engine.query(query_template))
            if solutions:
                sol = solutions[0]
                # Handle cases where a var might not be in the solution if query fails partially
                # or if prolog returns non-deterministic results where some vars are not bound.
                # For simple facts, this should be okay.
                return tuple(sol.get(var) for var in vars_to_extract)
            return None
        except Exception as e:
            print(f"Error getting prolog fact {query_template}: {e}")
            return None

    def get_prolog_list_fact(self, query_template, vars_to_extract):
        results = []
        try:
            for sol in self.prolog_engine.query(query_template):
                results.append(tuple(sol.get(var) for var in vars_to_extract))
            return results
        except Exception as e:
            print(f"Error getting prolog list fact {query_template}: {e}")
            return []

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
        # 1. Lấy thông tin nước đi cuối cùng một lần và chuyển đổi sang tọa độ Pygame
        py_from_last_move = None
        py_to_last_move = None
        last_move_query_result = list(self.prolog_engine.query("last_move(C1, R1, C2, R2)."))
        if last_move_query_result and last_move_query_result[0]:
            lm_data = last_move_query_result[0]
            # Kiểm tra xem tất cả các key có tồn tại và không phải None, và không phải trạng thái khởi tạo (0,0,0,0)
            if all(k in lm_data and lm_data[k] is not None for k in ['C1', 'R1', 'C2', 'R2']) and \
               not (lm_data.get('C1') == 0 and lm_data.get('R1') == 0 and lm_data.get('C2') == 0 and lm_data.get('R2') == 0):
                c1, r1, c2, r2 = lm_data['C1'], lm_data['R1'], lm_data['C2'], lm_data['R2']
                py_from_last_move = (8 - r1, c1 - 1) # (row_py, col_py)
                py_to_last_move = (8 - r2, c2 - 1)   # (row_py, col_py)

        for row in range(8):
            for col in range(8):
                rect = pygame.Rect(offset_x + col * self.cell_size, offset_y + row * self.cell_size, self.cell_size, self.cell_size)

                # 2a. Xác định màu nền của ô
                current_cell_bg_color = self.colors[(row + col) % 2] # Màu mặc định (trắng/đen)
                if py_from_last_move and (row, col) == py_from_last_move:
                    current_cell_bg_color = last_move_square_color
                if py_to_last_move and (row, col) == py_to_last_move: # Dùng 'if', không phải 'elif', vì ô đi và ô đến có thể trùng nhau
                    current_cell_bg_color = last_move_square_color
                
                # 2b. Vẽ nền ô
                pygame.draw.rect(self.screen, current_cell_bg_color, rect)

                # 2c. Vẽ các loại viền LÊN TRÊN màu nền
                prolog_row_to_check = 8 - row  # Chuyển đổi Py row sang Prolog row
                prolog_col_to_check = col + 1  # Chuyển đổi Py col sang Prolog col

                if (prolog_col_to_check, prolog_row_to_check) in self.highlighted_squares_prolog_coords:
                    pygame.draw.rect(self.screen, highlight_square_color, rect, border_thickness)
                if self.in_check_square == (row, col):
                    pygame.draw.rect(self.screen, king_in_check_color, rect, border_thickness)
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
        self.status_message = ""

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

    def update_highlighted_squares(self):
        """Cập nhật danh sách các ô cần highlight dựa trên quân cờ đang được chọn."""
        self.highlighted_squares_prolog_coords = [] # Xóa highlight cũ
        if self.selected_square is None:
            return

        from_row_py, from_col_py = self.selected_square
        piece_str_on_py_board = self.board_state[from_row_py][from_col_py]

        if not piece_str_on_py_board:
            return

        piece_type, color_prolog = self.parse_piece(piece_str_on_py_board)
        from_col_prolog = from_col_py + 1
        from_row_prolog = 8 - from_row_py

        # Đảm bảo Prolog có trạng thái bàn cờ hiện tại *TRƯỚC KHI* truy vấn
        self.assert_board_state()

        query = f"all_legal_moves_for_piece({piece_type}, {color_prolog}, {from_col_prolog}, {from_row_prolog}, TargetSquares)."
        try:
            solutions = list(self.prolog_engine.query(query))
            if solutions and solutions[0] and 'TargetSquares' in solutions[0] and isinstance(solutions[0]['TargetSquares'], list):
                raw_targets = solutions[0]['TargetSquares']
                processed_targets = []
                # Trả về list dạng [(C, R), (C, R), ...] 
                for target_item in raw_targets:
                    if isinstance(target_item, tuple) and len(target_item) == 2:
                        # Nếu là 1 cặp tupple (C, R) thì thêm vào list
                        processed_targets.append(target_item)
                    elif isinstance(target_item, str):
                        # If it's a string representation, try to parse it
                        try:
                            # Attempt to parse string format like ',(C, R)' or '(C, R)'
                            # Remove leading/trailing junk, then split by comma
                            cleaned_str = target_item.replace("(", "").replace(")", "").replace("'", "")
                            if cleaned_str.startswith(','): # Handle leading comma if present
                                cleaned_str = cleaned_str[1:]
                            parts = cleaned_str.split(',')
                            if len(parts) == 2:
                                col = int(parts[0].strip())
                                row = int(parts[1].strip())
                                processed_targets.append((col, row))
                            else:
                                print(f"DEBUG: Could not parse target string into tuple (unexpected parts): {target_item} -> {parts}") # DEBUG
                        except ValueError as e:
                            print(f"DEBUG: Error converting parts to int for target string {target_item}: {e}") # DEBUG
                        except Exception as e:
                            print(f"DEBUG: Unexpected error parsing target string {target_item}: {e}") # DEBUG
                    else:
                         print(f"DEBUG: Unexpected type in TargetSquares list: {type(target_item)} - {target_item}") # DEBUG

                self.highlighted_squares_prolog_coords = processed_targets
            else:
                self.highlighted_squares_prolog_coords = [] # Đảm bảo reset nếu không có kết quả
        except Exception as e:
            self.highlighted_squares_prolog_coords = []

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

        # --- Capture state BEFORE the move for undo ---
        move_data_for_history = {}
        save_history_this_time = False

        if is_ai_move: # AI luôn thực hiện một nước đi hoàn chỉnh
            save_history_this_time = True
        elif self.selected_square is not None: # Người chơi click lần thứ 2 (chọn ô đích)
            # Đây là thời điểm một nước đi sắp được thực hiện
            from_row_py_selected, _ = self.selected_square
            # Kiểm tra xem có phải click lại quân cũ hoặc quân khác cùng màu không (đã xử lý sau)
            # Nếu không phải các trường hợp đó, thì đây là một nỗ lực di chuyển
            if not ( (to_row_py, to_col_py) == self.selected_square or \
                     (self.board_state[to_row_py][to_col_py] and self.is_player_piece(self.board_state[to_row_py][to_col_py])) ):
                save_history_this_time = True

        if save_history_this_time:
            move_data_for_history = {
                "board_state_before_move": [row[:] for row in self.board_state], # Deep copy
                "turn_before_move": self.turn,
                "prolog_last_move": self.get_prolog_fact("last_move(C1,R1,C2,R2)", ['C1','R1','C2','R2']),
                "prolog_king_moved": self.get_prolog_list_fact("king_moved(ColorAtom)", ['ColorAtom']),
                "prolog_rook_moved": self.get_prolog_list_fact("rook_moved(ColorAtom, Col)", ['ColorAtom', 'Col']),
                "prolog_halfmove_clock": self.get_prolog_fact("halfmove_clock(N)", ['N']),
                "prolog_board_history_raw": self.get_prolog_fact("board_history(List)", ['List']),
                "in_check_square_before_move": self.in_check_square,
                "status_message_before_move": self.status_message,
                "game_over_before_move": self.game_over,
                "highlighted_squares_before_move": list(self.highlighted_squares_prolog_coords) # Deep copy
            }
            if move_data_for_history["prolog_board_history_raw"] and \
                isinstance(move_data_for_history["prolog_board_history_raw"][0], list):
                move_data_for_history["prolog_board_history"] = list(move_data_for_history["prolog_board_history_raw"][0]) # Deep copy
            else:
                move_data_for_history["prolog_board_history"] = []

        current_player_color_prolog = self.current_color() # "white" hoặc "black"

        if is_ai_move:
            from_row_prolog_move = ai_from_row
            from_col_prolog_move = ai_from_col
            piece_type_move = ai_piece_type
        else: # Nước đi của người
            if self.selected_square is None:
                # Người chơi chọn một quân cờ
                piece_on_square = self.board_state[to_row_py][to_col_py] # to_row_py, to_col_py là ô vừa click
                if piece_on_square:
                    if self.is_player_piece(piece_on_square):
                        self.selected_square = (to_row_py, to_col_py)
                        self.update_highlighted_squares() # Cập nhật highlight
                        self.status_message = ""
                    else:
                        self.status_message = "Cannot select this piece!"
                return # Đợi click thứ hai (ô đích)
            else:
                # Người chơi đã chọn quân và giờ chọn ô đích
                from_row_py_selected, from_col_py_selected = self.selected_square
                # self.highlighted_squares_prolog_coords = [] # Xóa highlight khi chuẩn bị di chuyển
                from_row_prolog_move = 8 - from_row_py_selected
                from_col_prolog_move = from_col_py_selected + 1
                
                piece_at_selected = self.board_state[from_row_py_selected][from_col_py_selected]
                if not piece_at_selected: 
                    self.selected_square = None
                    return
                piece_type_move, _ = self.parse_piece(piece_at_selected)

                if (to_row_py, to_col_py) == (from_row_py_selected, from_col_py_selected): # Click lại quân cũ
                    self.selected_square = None
                    self.highlighted_squares_prolog_coords = [] # Xóa highlight
                    return
                
                clicked_piece_on_board = self.board_state[to_row_py][to_col_py] # Ô đích vừa click
                if clicked_piece_on_board and self.is_player_piece(clicked_piece_on_board): # Click vào quân khác cùng màu
                    self.selected_square = (to_row_py, to_col_py) # Đổi quân chọn
                    self.update_highlighted_squares() # Cập nhật highlight cho quân mới
                    return

        # Đảm bảo Prolog có trạng thái bàn cờ hiện tại trước khi thử move_piece
        self.assert_board_state() 

        # Gọi move_piece trong Prolog
        query = f"move_piece({piece_type_move}, {current_player_color_prolog}, {from_col_prolog_move}, {from_row_prolog_move}, {to_col_prolog}, {to_row_prolog})."
        result = list(self.prolog_engine.query(query))

        if result: # move_piece thành công trong Prolog
            if save_history_this_time: # Chỉ thêm vào history nếu đó là một nỗ lực di chuyển hoàn chỉnh và thành công
                self.move_history.append(move_data_for_history)

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
                # Cập nhật board_state Python
                self.board_state[to_row_py][to_col_py] = f"{current_player_color_prolog[0]}_{promoted_to_piece_type}"

            if not is_ai_move: # Nếu là người chơi
                self.selected_square = None # Bỏ chọn quân
            self.highlighted_squares_prolog_coords = [] # Xóa highlight sau khi di chuyển

            # Sau khi di chuyển thành công và cập nhật, kiểm tra trạng thái game cho *đối thủ*
            opponent_color_prolog = "black" if current_player_color_prolog == "white" else "white"
            self.check_game_status_after_move(opponent_color_prolog)

            if not self.game_over:
                self.switch_turn()
            
        else: # move_piece thất bại trong Prolog
            if not is_ai_move: # Chỉ hiển thị "Invalid move!" cho người chơi
                self.status_message = "Invalid move!"
            self.selected_square = None # Bỏ chọn dù nước đi thất bại
            self.highlighted_squares_prolog_coords = [] # Xóa highlight nếu nước đi không hợp lệ
        

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
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do luật 50 nước
        if list(self.prolog_engine.query("draw_by_fifty_moves")):
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do lặp lại 3 lần
        if list(self.prolog_engine.query("draw_by_threefold_repetition")):
            game_over_stalemate_sound.play()
            self.game_over = True
            return

        # Hòa do không đủ quân chiếu bí
        if list(self.prolog_engine.query("insufficient_material")):
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
                self.status_message = "" # Thông báo lượt mặc định
            self.in_check_square = None

    # Kiểm tra xem quân cờ có phải là quân vua của đối thủ hay không
    def is_enemy_king(self, piece, enemy_color):
        piece_type, color = self.parse_piece(piece)
        return piece_type == "king" and color == enemy_color

    # Reset game state
    def reset_game(self):
        self.board_state = self.init_board_state() # Reset trạng thái bàn cờ Python
        self.reset_prolog_dynamic_facts() # Reset các fact động của Prolog
        self.assert_board_state() # Cập nhật Prolog với trạng thái bàn cờ mới

        self.turn = "w" # Lượt đi đầu tiên luôn là Trắng
        self.selected_square = None
        self.in_check_square = None # Xóa trạng thái vua bị chiếu
        self.status_message = "" # Thông báo lượt đi ban đầu
        self.highlighted_squares_prolog_coords = [] # Xóa highlight khi reset
        self.game_over = False
        self.move_history = [] # Xóa lịch sử nước đi
        # Không thay đổi self.play_with_ai, self.ai, self.ai_level, self.ai_color_char
        game_start_sound.play() # Phát âm thanh bắt đầu game mới

    # # Hoàn tác nước đi
    def undo_move(self):
        if not self.move_history:
            self.status_message = "No moves to undo!" # Cập nhật status message
            return

        num_undos = 1
        if self.play_with_ai:
            # Nếu đang chơi với AI:
            # - Nếu AI vừa đi (tức là self.turn là của người chơi), và có ít nhất 2 nước đi trong lịch sử,
            #   thì undo 2 lần (nước của AI và nước của người chơi trước đó).
            # - Ngược lại (người chơi vừa đi, hoặc chỉ còn 1 nước trong lịch sử), undo 1 lần.
            if self.turn != self.ai_color_char and len(self.move_history) >= 2:
                num_undos = 2
            # else: num_undos vẫn là 1

        if len(self.move_history) < num_undos:
            if self.play_with_ai and len(self.move_history) == 1 and num_undos == 2:
                num_undos = 1 # Nếu muốn undo 2 (AI) nhưng chỉ còn 1, thì undo 1
            elif len(self.move_history) < num_undos:
                self.status_message = "Not enough history to undo!"
                return

        restored_state_info_final = None # Sẽ lưu trạng thái của lần undo cuối cùng

        for i in range(num_undos):
            if not self.move_history: break # An toàn
            
            last_state_info = self.move_history.pop()
            if i == num_undos -1 : # Lần undo cuối cùng trong vòng lặp
                restored_state_info_final = last_state_info

            # Restore Python board state
            self.board_state = last_state_info["board_state_before_move"] # Đã là deep copy
            self.turn = last_state_info["turn_before_move"]
            self.in_check_square = last_state_info["in_check_square_before_move"]
            self.game_over = last_state_info["game_over_before_move"]
            self.highlighted_squares_prolog_coords = last_state_info.get("highlighted_squares_before_move", [])
            # self.status_message sẽ được đặt lại sau cùng

            # Reset current Prolog dynamic facts
            list(self.prolog_engine.query("retractall(last_move(_,_,_,_))"))
            list(self.prolog_engine.query("retractall(king_moved(_))"))
            list(self.prolog_engine.query("retractall(rook_moved(_,_))"))
            list(self.prolog_engine.query("retractall(halfmove_clock(_))"))
            list(self.prolog_engine.query("retractall(board_history(_))"))
            # piece_at sẽ được assert_board_state xử lý (nó có retractall riêng)

            # Assert the restored board state to Prolog
            self.assert_board_state()

            # Assert the specific dynamic facts from history

            if last_state_info["prolog_last_move"]:
                c1,r1,c2,r2 = last_state_info["prolog_last_move"]
                if all(v is not None for v in [c1,r1,c2,r2]):
                    list(self.prolog_engine.query(f"assertz(last_move({c1},{r1},{c2},{r2}))"))
                else:
                    list(self.prolog_engine.query("assertz(last_move(0,0,0,0))"))
            else:
                list(self.prolog_engine.query("assertz(last_move(0,0,0,0))"))

            for color_tuple in last_state_info.get("prolog_king_moved", []):
                color = color_tuple[0]
                if color: list(self.prolog_engine.query(f"assertz(king_moved({color}))"))

            for color_col_tuple in last_state_info.get("prolog_rook_moved", []):
                color_atom, col_val = color_col_tuple # Sửa tên biến để rõ ràng
                if color_atom and col_val: list(self.prolog_engine.query(f"assertz(rook_moved({color_atom},{col_val}))"))

            if last_state_info["prolog_halfmove_clock"] and last_state_info["prolog_halfmove_clock"][0] is not None:
                n = last_state_info["prolog_halfmove_clock"][0]
                list(self.prolog_engine.query(f"assertz(halfmove_clock({n}))"))
            else:
                list(self.prolog_engine.query("assertz(halfmove_clock(0))"))

            board_hist_list = last_state_info.get("prolog_board_history", [])
            if board_hist_list:
                list(self.prolog_engine.query(f"assertz(board_history({board_hist_list}))"))
            else:
                list(self.prolog_engine.query("assertz(board_history([]))"))

        self.highlighted_squares_prolog_coords = [] # Clear highlights after undo
        self.selected_square = None # Bỏ chọn quân sau khi undo

        # Sau khi hoàn tất tất cả các lần undo, cập nhật trạng thái game và thông báo
        if restored_state_info_final: # Nếu có ít nhất một lần undo thành công
            if not self.game_over: # Nếu game không ở trạng thái kết thúc sau khi undo
                self.check_game_status_after_move(self.current_color()) # Kiểm tra cho người chơi hiện tại
                if not self.game_over: # Nếu vẫn không kết thúc sau khi kiểm tra
                    self.status_message = ""
                # Nếu self.game_over là true sau check_game_status_after_move, status_message đã được đặt bởi nó
            else: # Nếu game ở trạng thái kết thúc sau khi undo (ví dụ: undo về chiếu bí)
                self.status_message = restored_state_info_final["status_message_before_move"]
                # Có thể cần gọi lại check_game_status để đảm bảo in_check_square được cập nhật đúng
                # nếu status_message là một dạng "Checkmate" hoặc "Check"
                if "checkmate" in self.status_message.lower() or "check!" in self.status_message.lower():
                     self.check_game_status_after_move(self.current_color())

        print(f"{num_undos} move(s) undone.")
        click_sound.play()

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