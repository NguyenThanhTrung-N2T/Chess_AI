import pygame
import os
from QuanCo import Piece

class Board:
    def __init__(self, screen, size=100):
        self.screen = screen
        self.size = size
        self.cell_size = size 
        self.colors = [(240, 217, 181), (181, 136, 99)]  # Màu bàn cờ
        self.selected_piece = None  # (piece, from_row, from_col)
        self.images = self.load_images()
        self.board = self.create_initial_board()

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
                rect = pygame.Rect(offset_x + col * self.size, offset_y + row * self.size, self.size, self.size)
                pygame.draw.rect(self.screen, color, rect)

    def draw_pieces(self, offset_x=0, offset_y=0):
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece:
                    color = 'w' if piece.mau == 'white' else 'b'
                    key = f"{color}_{piece.loai}"
                    image = self.images.get(key)
                    if image:
                        self.screen.blit(image, (offset_x + col * self.size, offset_y + row * self.size))

    def create_initial_board(self):
        def create_piece(loai, mau):
            return Piece(loai, mau)

        board = [[None for _ in range(8)] for _ in range(8)]

        # Đặt quân đen
        board[0] = [
            create_piece('rook', 'black'),
            create_piece('knight', 'black'),
            create_piece('bishop', 'black'),
            create_piece('queen', 'black'),
            create_piece('king', 'black'),
            create_piece('bishop', 'black'),
            create_piece('knight', 'black'),
            create_piece('rook', 'black')
        ]
        board[1] = [create_piece('pawn', 'black') for _ in range(8)]

        # Đặt quân trắng
        board[6] = [create_piece('pawn', 'white') for _ in range(8)]
        board[7] = [
            create_piece('rook', 'white'),
            create_piece('knight', 'white'),
            create_piece('bishop', 'white'),
            create_piece('queen', 'white'),
            create_piece('king', 'white'),
            create_piece('bishop', 'white'),
            create_piece('knight', 'white'),
            create_piece('rook', 'white')
        ]
        return board

    def handle_click(self, row, col):
        piece = self.get_piece_at(row, col)

        if self.selected_piece is None:
            if piece is not None:
                self.selected_piece = (piece, row, col)
                print(f"Selected piece at ({row}, {col}): {piece.loai} {piece.mau}")
        else:
            selected_piece, from_row, from_col = self.selected_piece
            # Di chuyển đến ô mới
            self.board[row][col] = selected_piece
            self.board[from_row][from_col] = None
            selected_piece.da_di = True
            print(f"Moved {selected_piece.loai} from ({from_row},{from_col}) to ({row},{col})")
            self.selected_piece = None

    def get_piece_at(self, row, col):
        if 0 <= row < 8 and 0 <= col < 8:
            return self.board[row][col]
        return None
