import pygame
import os
import chess  # Thư viện chess để quản lý bàn cờ 
from AI import ChessAI  # Thư viện AI để xử lý nước đi của máy

white_cell_color = (255,255,255)
black_cell_color = (127,164,209)
selected_square_color = (255, 223, 100)
king_in_check_color = (255,80,80)
highlight_square_color = (140, 230, 150)
last_move_square_color = (255, 165, 100)
border_thickness = 5
msg_pos = (726, 500)

class Board:
    def __init__(self, screen, size = 80):
        self.screen = screen
        self.size = int(size)
        self.cell_size = self.size
        self.colors = [white_cell_color, black_cell_color]  # Màu bàn cờ
        self.selected_square = None
        self.images = self.load_images()
        self.chess_board = chess.Board()
        self.king_in_check_square = None
        self.move_history = []
        self.font = pygame.font.Font('fonts/pixelmix.ttf', 15)
        self.status_message = ""
        self.game_over = False
        self.play_with_ai = False
        self.ai_player = chess.BLACK  # Máy chơi là bên nào
        self.ai_level = 2  # dễ = 1, trung bình = 2, khó = 3
        self.ai = ChessAI(level=self.ai_level)

    def is_white_turn(self):
        return 1 if self.chess_board.turn == chess.WHITE else 0

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
    
    def highlight_squares(self, offset_x, offset_y):
        if self.selected_square is None:
            return
        row, col = self.selected_square
        square = chess.square(col, 7 - row)
        possible_moves = [move.to_square for move in self.chess_board.legal_moves if move.from_square == square]
        if possible_moves:
            for move in possible_moves:
                row = 7 - move // 8
                col = move % 8
                if row < 0 or col < 0:
                    continue
                rect = pygame.Rect(offset_x + col * self.cell_size, offset_y + row * self.cell_size, self.cell_size, self.cell_size)
                pygame.draw.rect(self.screen, highlight_square_color, rect, border_thickness)
    
    def draw_board(self, offset_x, offset_y):
        for row in range(8):
            for col in range(8):
                color = self.colors[(row + col) % 2]
                rect = pygame.Rect(offset_x + col * self.cell_size, offset_y + row * self.cell_size, self.cell_size, self.cell_size)
                pygame.draw.rect(self.screen, color, rect)
                if self.king_in_check_square == (row, col):
                    pygame.draw.rect(self.screen, king_in_check_color, rect, border_thickness)
                if self.selected_square == (row, col):
                    pygame.draw.rect(self.screen, selected_square_color, rect, border_thickness)
        if self.move_history:
            last_move = chess.Move.from_uci(self.move_history[-1])
            from_square = last_move.from_square
            to_square = last_move.to_square

            from_row, from_col = 7 - chess.square_rank(from_square), chess.square_file(from_square)
            to_row, to_col = 7 - chess.square_rank(to_square), chess.square_file(to_square)

            from_rect = pygame.Rect(offset_x + from_col * self.cell_size, offset_y + from_row * self.cell_size, self.cell_size, self.cell_size)
            to_rect = pygame.Rect(offset_x + to_col * self.cell_size, offset_y + to_row * self.cell_size, self.cell_size, self.cell_size)

            pygame.draw.rect(self.screen, last_move_square_color, from_rect)
            pygame.draw.rect(self.screen, last_move_square_color, to_rect)  
        if self.status_message:
            msg = self.font.render(self.status_message, True, (255, 0, 0))
            if self.status_message not in ("White wins !", "Black wins !"):
                if self.status_message.startswith("Draw"):
                    msg = self.font.render(self.status_message[8:], True, (255, 0, 0))
                self.screen.blit(msg, msg_pos)

    def draw_pieces(self, offset_x, offset_y):
        for row in range(8):
            for col in range(8):
                square = chess.square(col, 7 - row)
                piece = self.chess_board.piece_at(square)
                if piece:
                    color = 'w' if piece.color == chess.WHITE else 'b'
                    type_map = {
                        chess.PAWN: 'pawn',
                        chess.KNIGHT: 'knight',
                        chess.BISHOP: 'bishop',
                        chess.ROOK: 'rook',
                        chess.QUEEN: 'queen',
                        chess.KING: 'king'
                    }
                    key = f"{color}_{type_map[piece.piece_type]}"
                    image = self.images.get(key)
                    if image:
                        self.screen.blit(image, (offset_x + col * self.cell_size, offset_y + row * self.cell_size))

    def handle_click(self, row, col):
        if self.game_over:
            return
        
        square = chess.square(col, 7 - row)

        if self.selected_square is None:
            if self.is_valid_selection(row, col):
                self.selected_square = (row, col)
                self.status_message = ">> Your king is in check" if self.chess_board.is_check() else ""
            else:
                self.status_message = ">> Can't select this cell"
        else:
            from_row, from_col = self.selected_square
            from_square = chess.square(from_col, 7 - from_row)
            to_square = square

            # Nếu vừa ấn quân mình thì không phải nước đi mà chỉ chọn quân khác
            selected_piece = self.chess_board.piece_at(from_square)
            clicked_piece = self.chess_board.piece_at(to_square)
            if clicked_piece and clicked_piece.color == selected_piece.color:
                self.selected_square = (row, col)
                return
            
            # Nước đi
            move = chess.Move(from_square, to_square)
            piece = self.chess_board.piece_at(from_square)
            to_rank = chess.square_rank(to_square)

            # Tạm thời dùng hậu làm quân phong mặc định để kiểm tra hợp lệ
            temp_move = chess.Move(from_square, to_square, promotion=chess.QUEEN)

            if piece.piece_type == chess.PAWN and to_rank in [0, 7] and temp_move in self.chess_board.legal_moves:
                # Nếu đi được và là phong cấp → cho người chơi chọn quân
                promotion_piece = self.show_promotion_menu()
                move = chess.Move(from_square, to_square, promotion=promotion_piece)
            else:
                move = chess.Move(from_square, to_square)

            if move in self.chess_board.legal_moves:
                self.move_history.append(move.uci())
                self.chess_board.push(move)

                if self.chess_board.is_check():
                    king_sq = self.chess_board.king(self.chess_board.turn)
                    row_check = 7 - chess.square_rank(king_sq)
                    col_check = chess.square_file(king_sq)
                    self.king_in_check_square = (row_check, col_check)
                    self.status_message = ">> Your king is in check "
                else:
                    self.king_in_check_square = None
                    self.status_message = ""

                if self.check_game_end():
                    self.game_over = True
                    return

                # Gọi AI nếu có
                if self.play_with_ai and self.chess_board.turn == self.ai_player:
                    ai_move = self.ai.select_move(self.chess_board)

                    if ai_move and ai_move in self.chess_board.legal_moves:
                        move_san = self.chess_board.san(ai_move)  # Gọi san() trước khi push()
                        self.chess_board.push(ai_move)
                        self.move_history.append(ai_move.uci())

                        if self.chess_board.is_check():
                            king_sq = self.chess_board.king(self.chess_board.turn)
                            row_check = 7 - chess.square_rank(king_sq)
                            col_check = chess.square_file(king_sq)
                            self.king_in_check_square = (row_check, col_check)
                            self.status_message = ">> Your king is in check "
                        else:
                            self.king_in_check_square = None
                            self.status_message = ""

                        if self.check_game_end():
                            self.game_over = True
                    else:
                        print(f"[LỖI] AI trả về nước đi không hợp lệ: {ai_move} - {self.chess_board.fen()}")

            else:
                self.status_message = ">> Invalid move "

            self.selected_square = None

    def is_valid_selection(self, row, col):
        square = chess.square(col, 7 - row)
        piece = self.chess_board.piece_at(square)
        return piece is not None and piece.color == self.chess_board.turn

    def check_game_end(self):
        if self.chess_board.is_checkmate():
            winner = "White" if not self.chess_board.turn else "Black"
            self.status_message = f"{winner} wins !"
            return True
        elif self.chess_board.is_stalemate():
            self.status_message = "Draw by Stalemate !"
            return True
        elif self.chess_board.is_insufficient_material():
            self.status_message = "Draw by Insufficient Material !"
            return True
        elif self.chess_board.can_claim_fifty_moves():
            self.status_message = "Draw by 50 Moves !"
            return True
        elif self.chess_board.can_claim_threefold_repetition():
            self.status_message = "Draw by Threefold Repetiton !"
            return True
        return False

    def reset_game(self):
        self.chess_board = chess.Board()
        self.move_history = []
        self.selected_square = None
        self.king_in_check_square = None
        self.status_message = ""
        self.game_over = False

    def undo_move(self):
        if self.move_history:
            self.move_history.pop()
            self.chess_board.pop()
            self.selected_square = None

            if self.chess_board.is_check():
                king_sq = self.chess_board.king(self.chess_board.turn)
                row_check = 7 - chess.square_rank(king_sq)
                col_check = chess.square_file(king_sq)
                self.king_in_check_square = (row_check, col_check)
                self.status_message = ">> Your king is in check!"
            else:
                self.king_in_check_square = None
                self.status_message = ""
        self.game_over = False

    def show_promotion_menu(self):
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
        color = 'w' if self.chess_board.turn == chess.WHITE else 'b'
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
                        return chess.QUEEN
                    elif btn_rook.collidepoint(mouse_pos):
                        return chess.ROOK
                    elif btn_bishop.collidepoint(mouse_pos):
                        return chess.BISHOP
                    elif btn_knight.collidepoint(mouse_pos):
                        return chess.KNIGHT
