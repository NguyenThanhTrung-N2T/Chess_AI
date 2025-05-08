import pygame
import os
import chess  # Thư viện chess để quản lý bàn cờ logic

class Board:
    def __init__(self, screen, size=100):
        self.screen = screen
        self.size = int(size)
        self.cell_size = self.size
        self.colors = [(240, 217, 181), (181, 136, 99)]  # Màu bàn cờ
        self.selected_square = None
        self.images = self.load_images()
        self.chess_board = chess.Board()
        self.king_in_check_square = None
        self.move_history = []
        self.font = pygame.font.SysFont("Arial", 24)
        self.status_message = ""
        self.game_over = False

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

        # Vẽ thông báo trạng thái dưới bàn cờ
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

    def show_promotion_menu(self):
        overlay = pygame.Surface((400, 400))  # lớp nền menu
        overlay.set_alpha(230)  # Độ trong suốt
        overlay.fill((50, 50, 50))  # Màu nền mờ
        
        button_font = pygame.font.SysFont("Arial", 28, bold=True)
        button_color = (100, 150, 250)
        text_color = (255, 255, 255)

        btn_queen = pygame.Rect(50, 50, 300, 50)
        btn_rook = pygame.Rect(50, 130, 300, 50)
        btn_bishop = pygame.Rect(50, 210, 300, 50)
        btn_knight = pygame.Rect(50, 290, 300, 50)

        while True:
            self.screen.blit(overlay, (0, 0))  # vẽ overlay lên màn hình chính
            pygame.draw.rect(self.screen, button_color, btn_queen)
            pygame.draw.rect(self.screen, button_color, btn_rook)
            pygame.draw.rect(self.screen, button_color, btn_bishop)
            pygame.draw.rect(self.screen, button_color, btn_knight)

            self.screen.blit(button_font.render("Phong Hậu (Q)", True, text_color), (90, 60))
            self.screen.blit(button_font.render("Phong Xe (R)", True, text_color), (90, 140))
            self.screen.blit(button_font.render("Phong Tượng (B)", True, text_color), (90, 220))
            self.screen.blit(button_font.render("Phong Mã (N)", True, text_color), (90, 300))

            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                if event.type == pygame.MOUSEBUTTONDOWN:
                    if btn_queen.collidepoint(event.pos):
                        return chess.QUEEN
                    elif btn_rook.collidepoint(event.pos):
                        return chess.ROOK
                    elif btn_bishop.collidepoint(event.pos):
                        return chess.BISHOP
                    elif btn_knight.collidepoint(event.pos):
                        return chess.KNIGHT


    def handle_click(self, row, col):
        if self.game_over:
            return

        square = chess.square(col, 7 - row)

        if self.selected_square is None:
            if self.is_valid_selection(row, col):
                self.selected_square = (row, col)
            else:
                self.status_message = ">> Không thể chọn ô này."
        else:
            from_row, from_col = self.selected_square
            from_square = chess.square(from_col, 7 - from_row)
            to_square = square
            move = chess.Move(from_square, to_square)

            piece = self.chess_board.piece_at(from_square)

            if piece.piece_type == chess.PAWN and chess.square_rank(to_square) in [0, 7]:
                # Hiển thị overlay menu chọn quân phong cấp ngay trên cửa sổ hiện tại
                promotion_piece = self.show_promotion_menu()

                # Không cần gọi lại set_mode!
                move = chess.Move(from_square, to_square, promotion=promotion_piece)
            else:
                move = chess.Move(from_square, to_square)





            if move in self.chess_board.legal_moves:
                if self.chess_board.is_castling(move):
                    self.status_message = ">> Nhập thành thành công!"

                self.move_history.append(self.chess_board.san(move))
                self.chess_board.push(move)

                if self.chess_board.is_check():
                    king_sq = self.chess_board.king(self.chess_board.turn)
                    row_check = 7 - chess.square_rank(king_sq)
                    col_check = chess.square_file(king_sq)
                    self.king_in_check_square = (row_check, col_check)
                    self.status_message = ">> Vua đang bị chiếu!"
                else:
                    self.king_in_check_square = None
                    if not self.chess_board.is_castling(move):
                        self.status_message = ""

                if self.check_game_end():
                    self.game_over = True
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
