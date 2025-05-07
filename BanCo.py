
import pygame
import os

class Board:
    def __init__(self, screen, size=50):
        self.screen = screen
        self.size = size
        self.colors = [(240, 217, 181), (181, 136, 99)]  # RGB cho #f0d9b5 và #b58863
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

    def draw_board(self, screen_width, screen_height):
        size = min(screen_width, screen_height)
        for row in range(8):
            for col in range(8):
                color = self.colors[(row + col) % 2]
                rect = pygame.Rect(col * self.size, row * self.size, self.size, self.size)
                pygame.draw.rect(self.screen, color, rect)
        return size

    def draw_pieces(self, size):
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece:
                    color = 'w' if piece['color'] == 'white' else 'b'
                    key = f"{color}_{piece['type']}"
                    image = self.images.get(key)
                    if image:
                        # Điều chỉnh kích thước quân cờ theo ô cờ
                        image = pygame.transform.scale(image, (size, size))
                        self.screen.blit(image, (col * size, row * size))

    def create_initial_board(self):
        def create_piece(type, color):
            return {'type': type, 'color': color}

        board = [[None for _ in range(8)] for _ in range(8)]
        # Đặt quân cờ đen
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
        # Đặt quân cờ trắng
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
