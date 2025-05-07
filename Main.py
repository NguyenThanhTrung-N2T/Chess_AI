import pygame
from BanCo import Board

def is_hovered(rect, mouse_pos):
    return rect.collidepoint(mouse_pos)

def draw_menu(screen):
    try:
        background_image = pygame.image.load("assets/te.jpg")
        background_image = pygame.transform.scale(background_image, (1000, 800))
    except pygame.error as e:
        print(f"Error loading image: {e}")
        return None

    clock = pygame.time.Clock()
    button_font = pygame.font.SysFont("Arial", 28, bold=True)
    title_font = pygame.font.SysFont("Arial", 50, bold=True)
    icon_size = (40, 40)

    # Load icon
    icon_pvp   = pygame.transform.scale(pygame.image.load("assets/nguoi_vs_nguoi.png"), icon_size)
    icon_ai_de = pygame.transform.scale(pygame.image.load("assets/ai_de.png"),       icon_size)
    icon_ai_tb = pygame.transform.scale(pygame.image.load("assets/ai_tb.png"),       icon_size)
    icon_ai_kho= pygame.transform.scale(pygame.image.load("assets/ai_kho.png"),      icon_size)

    # Định nghĩa các button
    buttons = [
        {"rect": pygame.Rect(342, 150, 320, 60), "text": "Player vs Player", "icon": icon_pvp,   "color": (100, 200, 100), "mode": "pvp"},
        {"rect": pygame.Rect(342, 250, 320, 60), "text": "Player vs AI (Easy)", "icon": icon_ai_de, "color": (100, 150, 250), "mode": "easy"},
        {"rect": pygame.Rect(342, 350, 320, 60), "text": "Player vs AI (Normal)", "icon": icon_ai_tb, "color": (250, 100, 100), "mode": "normal"},
        {"rect": pygame.Rect(342, 450, 320, 60), "text": "Player vs AI (Hard)", "icon": icon_ai_kho, "color": (250, 150, 100), "mode": "hard"},
    ]

    selected_mode = None
    running = True

    while running:
        screen.blit(background_image, (0, 0))
        mouse_pos   = pygame.mouse.get_pos()
        mouse_click = pygame.mouse.get_pressed()[0]

        # Vẽ tiêu đề
        title_text = title_font.render("Choose a game mode", True, (80, 84, 24))
        screen.blit(title_text, ((1000 - title_text.get_width()) // 2, 60))

        # Vẽ các button với hiệu ứng hover/click
        for btn in buttons:
            rect       = btn["rect"]
            base_color = btn["color"]
            hover_col  = tuple(min(255, c+30) for c in base_color)
            click_col  = tuple(max(0, c-30) for c in base_color)

            if is_hovered(rect, mouse_pos):
                color = click_col if mouse_click else hover_col
            else:
                color = base_color

            pygame.draw.rect(screen, color, rect, border_radius=10)
            screen.blit(btn["icon"], (rect.x + 10, rect.y + 10))
            txt = button_font.render(btn["text"], True, (255, 255, 255))
            # Căn chữ mặc định (cách 60px từ icon)
            screen.blit(txt, (rect.x + 60, rect.y + (rect.height - txt.get_height()) // 2))

        pygame.display.flip()
        clock.tick(60)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                return None
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                for btn in buttons:
                    if btn["rect"].collidepoint(mouse_pos):
                        selected_mode = btn["mode"]
                        running = False

    return selected_mode

def main():
    pygame.init()
    screen = pygame.display.set_mode((1000, 800))
    pygame.display.set_caption("Chess AI - Pygame")
    clock = pygame.time.Clock()

    # Chạy menu và lấy chế độ
    mode = draw_menu(screen)
    if mode is None:
        return
    print("Selected mode:", mode)

    # Khởi tạo bàn cờ
    board = Board(screen, size=100)

    # Vòng lặp chính
    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        screen.fill((239, 249, 253))

        # Vẽ bàn cờ và quân cờ
        board.draw_board(offset_x=0, offset_y=0)
        board.draw_pieces(offset_x=0, offset_y=0)

        # Vẽ panel bên phải (New, Undo, Reset)
        button_font = pygame.font.SysFont("Arial", 28, bold=True)
        button_color = (100, 150, 250)

        # New Game
        icon_new = pygame.transform.scale(pygame.image.load("assets/new_game.png"), (40,40))
        btn_new = pygame.Rect(810,100,180,50)
        pygame.draw.rect(screen, button_color, btn_new, border_radius=10)
        screen.blit(icon_new, (btn_new.x+10, btn_new.y+5))
        txt_new = button_font.render("New Game", True, (255,255,255))
        screen.blit(txt_new, (btn_new.x + 60, btn_new.y + (50 - txt_new.get_height())//2))

        # Undo
        icon_undo = pygame.transform.scale(pygame.image.load("assets/undo.png"), (40,40))
        btn_undo = pygame.Rect(810,180,180,50)
        pygame.draw.rect(screen, button_color, btn_undo, border_radius=10)
        screen.blit(icon_undo, (btn_undo.x+10, btn_undo.y+5))
        txt_undo = button_font.render("Undo", True, (255,255,255))
        # Căn giữa chữ với icon bù dịch +15px
        rect_undo = txt_undo.get_rect(center=(btn_undo.centerx+15, btn_undo.centery))
        screen.blit(txt_undo, rect_undo)

        # Reset
        icon_reset = pygame.transform.scale(pygame.image.load("assets/reset.png"), (40,40))
        btn_reset = pygame.Rect(810,260,180,50)
        pygame.draw.rect(screen, button_color, btn_reset, border_radius=10)
        screen.blit(icon_reset, (btn_reset.x+10, btn_reset.y+5))
        txt_reset = button_font.render("Reset", True, (255,255,255))
        rect_reset = txt_reset.get_rect(center=(btn_reset.centerx+10, btn_reset.centery))
        screen.blit(txt_reset, rect_reset)

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
