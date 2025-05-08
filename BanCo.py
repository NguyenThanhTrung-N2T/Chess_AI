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

            # Phong cấp nếu là tốt đi đến cuối bàn
            if self.chess_board.piece_at(from_square).piece_type == chess.PAWN:
                if chess.square_rank(to_square) in [0, 7]:
                    move = chess.Move(from_square, to_square, promotion=chess.QUEEN)

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
            winner = "Trắng" if not self.chess_board.turn else "Đen"
            self.status_message = f">> Chiếu hết! {winner} thắng!"
            return True
        elif self.chess_board.is_stalemate():
            self.status_message = ">> Hòa do bí!"
            return True
        elif self.chess_board.is_insufficient_material():
            self.status_message = ">> Hòa do không đủ quân!"
            return True
        elif self.chess_board.can_claim_fifty_moves():
            self.status_message = ">> Hòa do 50 nước không ăn quân và đi tốt!"
            return True
        elif self.chess_board.can_claim_threefold_repetition():
            self.status_message = ">> Hòa do lặp lại 3 lần!"
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
