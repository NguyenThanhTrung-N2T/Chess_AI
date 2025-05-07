
import pygame
from BanCo import Board

def draw_menu(screen):
    try:
        background_image = pygame.image.load("assets/te.jpg")
        background_image = pygame.transform.scale(background_image, (1000, 800))
        print("Background image loaded successfully")
    except pygame.error as e:
        print(f"Error loading image: {e}")
        return

    # Vẽ ảnh nền đầu tiên
    screen.blit(background_image, (0, 0))  # VẼ ẢNH NỀN TRƯỚC

    # Sau đó vẽ các thành phần giao diện khác
    font = pygame.font.SysFont("Arial", 50, bold=True)
    title_text = font.render("Choose a game mode", True, (80, 84, 24))
    button_font = pygame.font.SysFont("Arial", 28, bold=True)

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
    button_pvp = pygame.Rect(342, 150, 320, 60)
    button_ai_de = pygame.Rect(342, 250, 320, 60)
    button_ai_tb = pygame.Rect(342, 350, 320, 60)
    button_ai_kho = pygame.Rect(342, 450, 320, 60)

    pygame.draw.rect(screen, (100, 200, 100), button_pvp, border_radius=10)
    pygame.draw.rect(screen, (100, 150, 250), button_ai_de, border_radius=10)
    pygame.draw.rect(screen, (250, 100, 100), button_ai_tb, border_radius=10)
    pygame.draw.rect(screen, (250, 150, 100), button_ai_kho, border_radius=10)

    # Vẽ icon và text trong button Người vs Người
    screen.blit(icon_pvp, (button_pvp.x + 10, button_pvp.y + 10))
    text_pvp = button_font.render("Player vs Player", True, (255, 255, 255))
    screen.blit(text_pvp, (button_pvp.x + 60, button_pvp.y + 15))

    # AI dễ
    screen.blit(icon_ai_de, (button_ai_de.x + 10, button_ai_de.y + 10))
    text_ai = button_font.render("Player vs AI (Easy)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_de.x + 60, button_ai_de.y + 15))

    # AI trung bình
    screen.blit(icon_ai_tb, (button_ai_tb.x + 10, button_ai_tb.y + 10))
    text_ai = button_font.render("Player vs AI (Normal)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_tb.x + 60, button_ai_tb.y + 15))

    # AI khó
    screen.blit(icon_ai_kho, (button_ai_kho.x + 10, button_ai_kho.y + 10))
    text_ai = button_font.render("Player vs AI (Hard)", True, (255, 255, 255))
    screen.blit(text_ai, (button_ai_kho.x + 60, button_ai_kho.y + 15))

    pygame.display.flip()

def main():
    pygame.init()
    screen = pygame.display.set_mode((1000,800))
    pygame.display.set_caption("Chess AI - Pygame")
    clock = pygame.time.Clock()

    # ======= MENU HIỂN THỊ =======
    draw_menu(screen)

    # Chờ khoảng 2 giây rồi vào bàn cờ (hoặc thay bằng click sau này)
    pygame.time.wait(2000)

    board = Board(screen, size=100)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        screen.fill((239, 249, 253))  # Xóa nền trước khi vẽ lại
        # Vẽ bàn cờ từ góc trái
        board.draw_board(offset_x=0, offset_y=0)
        board.draw_pieces(offset_x=0, offset_y=0)

        # Vẽ panel phải (200px còn lại)
        pygame.draw.rect(screen, (239, 249,253), (800, 0, 200, 800))  # Khung bên phải

        # Load icon cho New Game
        icon_newgame = pygame.image.load("assets/new_game.png")
        icon_newgame = pygame.transform.scale(icon_newgame, (40, 40))

        # Vẽ button New Game
        button_font = pygame.font.SysFont("Arial", 28, bold=True)
        button_color = (100, 150, 250)
        button_newgame = pygame.Rect(810, 100, 180, 50)
        pygame.draw.rect(screen, button_color, button_newgame, border_radius=10)
        screen.blit(icon_newgame, (button_newgame.x + 10, button_newgame.y + 5))
        text_newgame = button_font.render("New Game", True, (255, 255, 255))
        screen.blit(text_newgame, (button_newgame.x + 60, button_newgame.y + 10))

        # Load icon cho Undo và Reset
        icon_undo = pygame.image.load("assets/undo.png")
        icon_reset = pygame.image.load("assets/reset.png")
        icon_undo = pygame.transform.scale(icon_undo, (40, 40))
        icon_reset = pygame.transform.scale(icon_reset, (40, 40))

        # Font giống như các button cũ
        button_font = pygame.font.SysFont("Arial", 28, bold=True)

        # Undo button
        button_undo = pygame.Rect(810, 180, 180, 50)
        pygame.draw.rect(screen, button_color, button_undo, border_radius=10)
        screen.blit(icon_undo, (button_undo.x + 10, button_undo.y + 5))
        text_undo = button_font.render("Undo", True, (255, 255, 255))
        text_undo_rect = text_undo.get_rect(center=(button_undo.centerx + 15, button_undo.centery))
        screen.blit(text_undo, text_undo_rect)

        # Reset button
        button_reset = pygame.Rect(810, 260, 180, 50)
        pygame.draw.rect(screen, button_color, button_reset, border_radius=10)
        screen.blit(icon_reset, (button_reset.x + 10, button_reset.y + 5))
        text_reset = button_font.render("Reset", True, (255, 255, 255))
        text_reset_rect = text_reset.get_rect(center=(button_reset.centerx + 10, button_reset.centery))
        screen.blit(text_reset, text_reset_rect)

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
