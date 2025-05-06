
import pygame
from BanCo import Board

def main():
    pygame.init()
    size = 50
    screen = pygame.display.set_mode((size * 8, size * 8))
    pygame.display.set_caption("Chess AI - Pygame")
    clock = pygame.time.Clock()
    board = Board(screen, size)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        board.draw_board()
        board.draw_pieces()
        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
