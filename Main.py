import pygame
import os
from BanCo import Board  # Import lớp Board của bạn
from AI import ChessAI  # Import lớp AI của bạn

screen_w = 1000
screen_h = 700

button_text_size = 16
title_text_size = 30
icon_size = (40, 40)

cell_size = 80
offset_x_board = 30
offset_y_board = 30
offset_x_piece = 30
offset_y_piece = 20
board_size = (699,699)
background_color = (239, 249, 253)

x_btn = 760
y_new_btn = 100
y_undo_btn = 180
y_reset_btn = 260
btn_w = 180
btn_h = 50
btn_text_color = (255,255,255)

game_over_text_color = (255, 215, 0)

def is_hovered(rect, mouse_pos):
    return rect.collidepoint(mouse_pos)

def mouse_in_board(mouse_x, mouse_y):
    if mouse_x < offset_x_board or mouse_x > board_size[0] - offset_x_board:
        return False
    if mouse_y < offset_y_board or mouse_y > board_size[1] - offset_y_board:
        return False
    return True 

def draw_menu(screen):
    try:
        background_image = pygame.image.load("assets/te.jpg")
        background_image = pygame.transform.scale(background_image, (screen_w,screen_h))
    except pygame.error as e:
        print(f"Error loading image: {e}")
        return None

    clock = pygame.time.Clock()
    button_font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    title_font = pygame.font.Font("fonts/pixelmix_bold.ttf", title_text_size)

    # Load icon
    icon_pvp = pygame.transform.scale(pygame.image.load("assets/nguoi_vs_nguoi.png"), icon_size)
    icon_ai_de = pygame.transform.scale(pygame.image.load("assets/ai_de.png"), icon_size)
    icon_ai_tb = pygame.transform.scale(pygame.image.load("assets/ai_tb.png"), icon_size)
    icon_ai_kho = pygame.transform.scale(pygame.image.load("assets/ai_kho.png"), icon_size)

    # Định nghĩa các button
    buttons = [
        {"rect": pygame.Rect(290, 150, 320, 60), "text": "Player vs Player", "icon": icon_pvp,   "color": (100, 200, 100), "mode": "pvp"},
        {"rect": pygame.Rect(290, 250, 320, 60), "text": "Player vs AI (Easy)", "icon": icon_ai_de, "color": (100, 150, 250), "mode": "easy"},
        {"rect": pygame.Rect(290, 350, 320, 60), "text": "Player vs AI (Normal)", "icon": icon_ai_tb, "color": (250, 100, 100), "mode": "normal"},
        {"rect": pygame.Rect(290, 450, 320, 60), "text": "Player vs AI (Hard)", "icon": icon_ai_kho, "color": (250, 150, 100), "mode": "hard"},
    ]

    selected_mode = None
    running = True

    while running:
        screen.blit(background_image, (0, 0))
        mouse_pos   = pygame.mouse.get_pos()
        mouse_click = pygame.mouse.get_pressed()[0]

        title_text = title_font.render("CHOOSE A GAME MODE", True, (80, 84, 24))
        screen.blit(title_text, ((screen_w - title_text.get_width()) // 2, 60))

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
    screen = pygame.display.set_mode((screen_w,screen_h), pygame.SCALED)
    pygame.display.set_caption("Chess Game")
    clock = pygame.time.Clock()

    mode = draw_menu(screen)

    if mode is None:
        return

    print("Selected mode:", mode)

    board = Board(screen, cell_size)  # Tạo bàn cờ mới

    if mode == "easy":
        board.ai_level = 1
        board.play_with_ai = True
        board.ai = ChessAI(level=1)
    elif mode == "normal":
        board.ai_level = 2
        board.play_with_ai = True
        board.ai = ChessAI(level=2)
    elif mode == "hard":
        board.ai_level = 3
        board.play_with_ai = True
        board.ai = ChessAI(level=3)

    btn_color = (140, 120, 200)
    button_font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    txt_new = button_font.render("New Game", True, btn_text_color)
    txt_undo = button_font.render("Undo", True, btn_text_color)
    txt_reset = button_font.render("Reset", True, btn_text_color)

    # Thiết lập các nút điều khiển
    icon_new = pygame.transform.scale(pygame.image.load("assets/new_game.png"), icon_size)
    icon_undo = pygame.transform.scale(pygame.image.load("assets/undo.png"), icon_size)
    icon_reset = pygame.transform.scale(pygame.image.load("assets/reset.png"), icon_size)

    btn_new = pygame.Rect(x_btn, y_new_btn, btn_w, btn_h)
    btn_undo = pygame.Rect(x_btn, y_undo_btn, btn_w, btn_h)
    btn_reset = pygame.Rect(x_btn, y_reset_btn, btn_w, btn_h)

    # Thiết lập viền bàn cờ
    board_border = pygame.image.load("images/board_border.png")
    board_border = pygame.transform.scale(board_border, board_size)
    board_border_rect = board_border.get_rect(topleft = (0,0))
    
    running = True
    while running:
        #background and buttons
        mouse_pos = pygame.mouse.get_pos()
        def draw_button(btn, txt, icon = 0):
            color = tuple(c + 20 for c in btn_color) if is_hovered(btn, mouse_pos) else btn_color
            pygame.draw.rect(screen, color, btn)
            screen.blit(txt, txt.get_rect(center=btn.center))
            #screen.blit(icon, btn)
        draw_button(btn_new, txt_new)
        draw_button(btn_undo, txt_undo)
        draw_button(btn_reset, txt_reset)

        #board
        screen.blit(board_border, board_border_rect) 
        board.draw_board(offset_x = offset_x_board, offset_y = offset_y_board) 
        board.highlight_squares(offset_x = offset_x_board, offset_y = offset_y_board)
        board.draw_pieces(offset_x = offset_x_piece, offset_y = offset_y_piece)

        # Hiển thị thông báo kết quả khi game kết thúc
        if board.game_over:
            # Phần 1: Kết thúc trò chơi
            game_over_message = ">> End Game <<"
            game_over_text = button_font.render(game_over_message, True, game_over_text_color)
            screen.blit(game_over_text, (350, 350))  # Hiển thị "Kết thúc" ở giữa bàn cờ

            # Phần 2: Thông báo người thắng hoặc hòa
            result_message = board.status_message
            result_text = button_font.render(result_message, True, game_over_text_color)
            screen.blit(result_text, (350, 390))  # Hiển thị kết quả thắng hoặc hòa ở dưới "Kết thúc"
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mouse_x, mouse_y = pygame.mouse.get_pos()

                # Xử lý sự kiện nhấn vào ô cờ
                if mouse_in_board(mouse_x, mouse_y):
                    row = (mouse_y - offset_y_board) // board.cell_size
                    col = (mouse_x - offset_x_board) // board.cell_size
                    board.handle_click(row, col)

                # Xử lý sự kiện nhấn nút
                elif btn_new.collidepoint(mouse_x, mouse_y):
                    mode = draw_menu(screen)
                    # Reset lại bàn cờ với chế độ mới
                    board = Board(screen, cell_size)
                    if mode == "easy":
                        board.ai_level = 1
                        board.play_with_ai = True
                        board.ai = ChessAI(level=1)
                    elif mode == "normal":
                        board.ai_level = 2
                        board.play_with_ai = True
                        board.ai = ChessAI(level=2)
                    elif mode == "hard":
                        board.ai_level = 3
                        board.play_with_ai = True
                        board.ai = ChessAI(level=3)

                elif btn_undo.collidepoint(mouse_x, mouse_y):
                    board.undo_move()  # Hoàn tác nước đi

                elif btn_reset.collidepoint(mouse_x, mouse_y):
                    board.reset_game()  # Đặt lại trò chơi

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
