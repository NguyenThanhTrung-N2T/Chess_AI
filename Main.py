import pygame
from BanCo import Board

def draw_menu(screen):
    font = pygame.font.SysFont("Arial", 36, bold=True)
    title_text = font.render("Choose a game mode", True, (0, 0, 0))
    button_font = pygame.font.SysFont("Arial", 28, bold=True)

    screen.fill((240, 240, 240))
    screen.blit(title_text, (screen.get_width() // 2 - title_text.get_width() // 2, 60))

    # Load ảnh biểu tượng
    icon_pvp = pygame.image.load("assets/nguoi_vs_nguoi.png")
    icon_ai_de = pygame.image.load("assets/ai_de.png")
    icon_ai_tb = pygame.image.load("assets/ai_tb.png")
    icon_ai_kho = pygame.image.load("assets/ai_kho.png")
    icon_pvp = pygame.transform.scale(icon_pvp, (40, 40))
    icon_ai_de = pygame.transform.scale(icon_ai_de, (40, 40))
    icon_ai_tb = pygame.transform.scale(icon_ai_tb, (40, 40))
    icon_ai_kho = pygame.transform.scale(icon_ai_kho, (40, 40))

    # Vị trí và vẽ button
    button_pvp = pygame.Rect(165, 150, 320, 60)
    button_ai_de = pygame.Rect(165, 250, 320, 60)
    button_ai_tb = pygame.Rect(165, 350, 320, 60)
    button_ai_kho = pygame.Rect(165, 450, 320, 60)

    pygame.draw.rect(screen, (100, 200, 100), button_pvp, border_radius=10)
    pygame.draw.rect(screen, (100, 150, 250), button_ai_de, border_radius=10)
    pygame.draw.rect(screen, (250, 100, 100), button_ai_tb, border_radius=10)
    pygame.draw.rect(screen, (250, 150, 100), button_ai_kho, border_radius=10)

    # Vẽ icon và text trong button Người vs Người
    screen.blit(icon_pvp, (button_pvp.x + 10, button_pvp.y + 10))
    text_pvp = button_font.render("Player vs Player", True, (255, 255, 255))
    screen.blit(text_pvp, (button_pvp.x + 60, button_pvp.y + 15))

    # Vẽ icon và text trong button Người vs Máy
    # AI dễ
    screen.blit(icon_ai_de, (button_ai_de.x + 10, button_ai_de.y + 10))
    text_ai = button_font.render("Player vs AI(Easy)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_de.x + 60, button_ai_de.y + 15))
    # AI trung bình
    screen.blit(icon_ai_tb, (button_ai_tb.x + 10, button_ai_tb.y + 10))
    text_ai = button_font.render("Player vs AI(Normal)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_tb.x + 60, button_ai_tb.y + 15))
    # AI khó
    screen.blit(icon_ai_kho, (button_ai_kho.x + 10, button_ai_kho.y + 10))
    text_ai = button_font.render("Player vs AI(Hard)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_kho.x + 60, button_ai_kho.y + 15))

    pygame.display.flip()


def main():
    pygame.init()
    size = 80  # Kích thước mỗi ô cờ
    screen = pygame.display.set_mode((size * 8, size * 8))
    pygame.display.set_caption("Chess AI - Pygame")
    clock = pygame.time.Clock()

    # ======= MENU HIỂN THỊ =======
    draw_menu(screen)

    # Chờ khoảng 2 giây rồi vào bàn cờ (hoặc bạn có thể thay bằng sự kiện click sau này)
    pygame.time.wait(2000)

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
