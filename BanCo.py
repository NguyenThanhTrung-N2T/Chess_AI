import pygame
import os
import chess  # Thư viện chess để quản lý bàn cờ 
from AI import ChessAI  # Thư viện AI để xử lý nước đi của máy

class Board:
    def __init__(self, screen, size=80):
        self.screen = screen
        self.size = int(size)
        self.cell_size = self.size
        self.colors = [(240, 217, 181), (181, 136, 99)]  # Màu bàn cờ
        self.selected_square = None
        self.images = self.load_images()
        self.chess_board = chess.Board()
        self.king_in_check_square = None
        self.move_history = []
        self.font = pygame.font.Font("fonts/pixelmix.ttf", 16)
        self.status_message = ""
        self.game_over = False
        self.play_with_ai = False
        self.ai_player = chess.BLACK  # Máy chơi là bên nào
        self.ai_level = 2  # dễ = 1, trung bình = 2, khó = 3
        self.ai = ChessAI(level=self.ai_level)

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

    def draw_board(self, offset_x=0, offset_y=0):
        for row in range(8):
            for col in range(8):
                color = self.colors[(row + col) % 2]
                rect = pygame.Rect(offset_x + col * self.cell_size, offset_y + row * self.cell_size, self.cell_size, self.cell_size)
                pygame.draw.rect(self.screen, color, rect)

                if self.selected_square == (row, col):
                    pygame.draw.rect(self.screen, (255, 0, 0), rect, 5)

                if self.king_in_check_square == (row, col):
                    pygame.draw.rect(self.screen, (255, 255, 0), rect, 5)

        if self.status_message:
            msg = self.font.render(self.status_message, True, (255, 0, 0))
            self.screen.blit(msg, (10, 8 * self.cell_size + 10))

    def draw_pieces(self, offset_x=0, offset_y=0):
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
            else:
                self.status_message = ">> Can't select this cell"
        else:
            from_row, from_col = self.selected_square
            from_square = chess.square(from_col, 7 - from_row)
            to_square = square
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
                self.move_history.append(self.chess_board.san(move))
                self.chess_board.push(move)

                if self.chess_board.is_check():
                    king_sq = self.chess_board.king(self.chess_board.turn)
                    row_check = 7 - chess.square_rank(king_sq)
                    col_check = chess.square_file(king_sq)
                    self.king_in_check_square = (row_check, col_check)
                    self.status_message = ">> King is being checkmated!"
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
                        self.move_history.append(move_san)

                        if self.chess_board.is_check():
                            king_sq = self.chess_board.king(self.chess_board.turn)
                            row_check = 7 - chess.square_rank(king_sq)
                            col_check = chess.square_file(king_sq)
                            self.king_in_check_square = (row_check, col_check)
                            self.status_message = ">> Vua đang bị chiếu!"
                        else:
                            self.king_in_check_square = None
                            self.status_message = ""

                        if self.check_game_end():
                            self.game_over = True
                    else:
                        print(f"[LỖI] AI trả về nước đi không hợp lệ: {ai_move} - {self.chess_board.fen()}")

            else:
                self.status_message = ">> Nước đi không hợp lệ."

            self.selected_square = None

    def is_valid_selection(self, row, col):
        square = chess.square(col, 7 - row)
        piece = self.chess_board.piece_at(square)
        return piece is not None and piece.color == self.chess_board.turn

    def check_game_end(self):
        if self.chess_board.is_checkmate():
            winner = "White" if not self.chess_board.turn else "Black"
            self.status_message = f">> {winner} win !"
            return True
        elif self.chess_board.is_stalemate():
            self.status_message = ">> Draw by stalemate !"
            return True
        elif self.chess_board.is_insufficient_material():
            self.status_message = ">> Draw by enough pieces !"
            return True
        elif self.chess_board.can_claim_fifty_moves():
            self.status_message = ">> Draw by 50 moves !"
            return True
        elif self.chess_board.can_claim_threefold_repetition():
            self.status_message = ">> Draw by threefold repetiton !"
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

            if self.chess_board.is_check():
                king_sq = self.chess_board.king(self.chess_board.turn)
                row_check = 7 - chess.square_rank(king_sq)
                col_check = chess.square_file(king_sq)
                self.king_in_check_square = (row_check, col_check)
                self.status_message = ">> Vua đang bị chiếu!"
            else:
                self.king_in_check_square = None
                self.status_message = ""

    def show_promotion_menu(self):
        overlay = pygame.Surface((400, 400))
        overlay.set_alpha(230)
        overlay.fill((241, 218, 91))

        button_font = pygame.font.Font("fonts/pixelmix.ttf", 16)
        button_color = (247, 81, 90)
        text_color = (255, 255, 255)

        # Tính toán toạ độ dọc để căn giữa danh sách 4 nút
        btn_width = 300
        btn_height = 50
        gap = 20  # khoảng cách giữa các nút
        total_height = 4 * btn_height + 3 * gap
        start_y = (400 - total_height) // 2

        # Căn giữa theo chiều ngang
        start_x = (400 - btn_width) // 2

        btn_queen  = pygame.Rect(start_x, start_y + 0 * (btn_height + gap), btn_width, btn_height)
        btn_bishop = pygame.Rect(start_x, start_y + 1 * (btn_height + gap), btn_width, btn_height)
        btn_knight = pygame.Rect(start_x, start_y + 2 * (btn_height + gap), btn_width, btn_height)
        btn_rook   = pygame.Rect(start_x, start_y + 3 * (btn_height + gap), btn_width, btn_height)

        # Load và resize icon
        queen_icon = pygame.image.load("images/queen.png")
        rook_icon = pygame.image.load("images/rook.png")
        bishop_icon = pygame.image.load("images/bishop.png")
        knight_icon = pygame.image.load("images/knight.png")

        icon_size = (32, 32)
        queen_icon = pygame.transform.scale(queen_icon, icon_size)
        rook_icon = pygame.transform.scale(rook_icon, icon_size)
        bishop_icon = pygame.transform.scale(bishop_icon, icon_size)
        knight_icon = pygame.transform.scale(knight_icon, icon_size)
        while True:
            self.screen.blit(overlay, (0, 0))
            pygame.draw.rect(self.screen, button_color, btn_queen)
            pygame.draw.rect(self.screen, button_color, btn_rook)
            pygame.draw.rect(self.screen, button_color, btn_bishop)
            pygame.draw.rect(self.screen, button_color, btn_knight)
            def draw_button(rect, icon, text):
                text_surface = button_font.render(text, True, text_color)
                spacing = 10
                total_width = icon.get_width() + spacing + text_surface.get_width()

                # Tính điểm bắt đầu để căn giữa cụm icon + text trong button
                start_x = rect.x + (rect.width - total_width) // 2
                icon_y = rect.y + (rect.height - icon.get_height()) // 2
                text_y = rect.y + (rect.height - text_surface.get_height()) // 2

                self.screen.blit(icon, (start_x, icon_y))
                self.screen.blit(text_surface, (start_x + icon.get_width() + spacing, text_y))

            draw_button(btn_queen, queen_icon, "Queen")
            draw_button(btn_rook, rook_icon, "Rook")
            draw_button(btn_bishop, bishop_icon, "Bishop")
            draw_button(btn_knight, knight_icon, "Knight")

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
