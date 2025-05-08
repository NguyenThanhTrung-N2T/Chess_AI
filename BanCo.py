import pygame
import os
import chess  # Thư viện chess để quản lý bàn cờ logic

class Board:
    def __init__(self, screen, size=100):
        self.screen = screen
        self.size = int(size)  # Đảm bảo size là số nguyên
        self.cell_size = self.size  # Nếu muốn thay đổi, thay ở đây
        self.colors = [(240, 217, 181), (181, 136, 99)]  # Màu bàn cờ
        self.selected_square = None  # (row, col)
        self.images = self.load_images()
        self.chess_board = chess.Board()  # Chỉ dùng chess.Board
        self.king_in_check_square = None  # Vị trí vua bị chiếu
        self.move_history = []  # Lịch sử nước đi

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

                # Viền đỏ cho quân đang được chọn
                if self.selected_square == (row, col):
                    pygame.draw.rect(self.screen, (255, 0, 0), rect, 5)

                # Viền vàng nếu vua bị chiếu
                if self.king_in_check_square == (row, col):
                    pygame.draw.rect(self.screen, (255, 255, 0), rect, 5) # Viền vàng cho vua bị chiếu

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
        square = chess.square(col, 7 - row)

        if self.selected_square is None:
            if self.is_valid_selection(row, col):
                self.selected_square = (row, col)
                print(f"Selected piece at ({row},{col})")
            else:
                print(f"Không thể chọn ô ({row},{col})")
        else:
            from_row, from_col = self.selected_square
            from_square = chess.square(from_col, 7 - from_row)
            to_square = square
            move = chess.Move(from_square, to_square)

            if move in self.chess_board.legal_moves:
                self.move_history.append(self.chess_board.san(move))
                self.chess_board.push(move)
                print(f"Moved from ({from_row},{from_col}) to ({row},{col})")

                if self.chess_board.is_check():
                    king_sq = self.chess_board.king(self.chess_board.turn)
                    row_check = 7 - chess.square_rank(king_sq)
                    col_check = chess.square_file(king_sq)
                    self.king_in_check_square = (row_check, col_check)
                else:
                    self.king_in_check_square = None
            else:
                print(f"Nước đi không hợp lệ từ ({from_row},{from_col}) đến ({row},{col})")

            self.selected_square = None

    def is_valid_selection(self, row, col):
        square = chess.square(col, 7 - row)
        piece = self.chess_board.piece_at(square)
        return piece is not None and piece.color == self.chess_board.turn

    def reset_game(self):
        self.chess_board = chess.Board()
        self.move_history = []  # Reset lịch sử
        self.selected_square = None
        self.king_in_check_square = None

    def undo_move(self):
        if self.move_history:
            last_move = self.move_history.pop()  # Lấy nước đi cuối cùng
            print(f"Undoing move: {last_move}")

            # Quay lại bàn cờ trước nước đi này
            self.chess_board.pop()  # Hoàn tác nước đi cuối cùng

            # Cập nhật lại các thông tin về quân vua bị chiếu
            if self.chess_board.is_check():
                king_sq = self.chess_board.king(self.chess_board.turn)
                row_check = 7 - chess.square_rank(king_sq)
                col_check = chess.square_file(king_sq)
                self.king_in_check_square = (row_check, col_check)
            else:
                self.king_in_check_square = None
