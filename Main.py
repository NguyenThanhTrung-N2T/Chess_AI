import pygame
import os
from BanCo import Board  # Import lớp Board của bạn
from AI import ChessAI  # Import lớp AI của bạn

screen_w = 1000
screen_h = 700

button_text_size = 16
title_text_size = 30

cell_size = 80
offset_x_board = 30
offset_y_board = 30
offset_x_piece = 30
offset_y_piece = 20
board_size = (699,699)
background_color = (239, 249, 253)

btn_w = 180
btn_h = 50
btn_text_color = (255,255,255)


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
    icon_size = (40, 40)
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
        screen.blit(title_text, ((screen_w - title_text.get_width() -96 ) // 2, 60))

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
            txt = button_font.render(btn["text"], True, btn_text_color)
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

popup_color = (220, 220, 220)
popup_border_color = tuple(x - 20 for x in popup_color)
popup_text_color = (35, 35, 35)
popup_btn_color =  (240, 240, 240)
popup_x = 220
popup_y = 590
popup_w, popup_h = 260, 300
popup_start_y = 590
popup_end_y = 200
popup_btn_w, popup_btn_h = 100, 50
gap_popup_n_btn = 12
gap_popup_n_txt = 10
popup_btn_text_size = 13
    

def result_popup(screen, board):
    global popup_y
    speed = 50
    winner = "w" if not board.chess_board.turn else "b"
    font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    font2 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size - 3)
    font3 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size + 3)
    if winner == "b":
        popup_color = (112, 128, 159)
        popup_border_color = (popup_color[0]-20, popup_color[1]-20, popup_color[2]-20)
        popup_text_color = (30, 30, 15)
        popup_btn_color = (185, 215, 230)
    else:
        popup_color = (220, 220, 220)
        popup_border_color = (popup_color[0]-20, popup_color[1]-20, popup_color[2]-20)
        popup_text_color = (35, 35, 35)
        popup_btn_color =  (240, 240, 240)

    mouse_pos = pygame.mouse.get_pos()

    btn_back = pygame.Rect(0,0,0,0)
    btn_playagain = pygame.Rect(0,0,0,0)
    
    #load
    def render_popup(popup_y, btn_back, btn_playagain):
        text = font.render("END GAME", False, popup_text_color)
        text1 = font3.render(board.status_message, False, popup_text_color)
        text_playagain = font2.render("Play again", False, popup_text_color)
        text_back = font2.render("Back", False, popup_text_color)

        rect_popup = pygame.Rect(popup_x, popup_y, popup_w, popup_h)
        rect_popup_shadow = rect_popup.move(5,5)
        rect_text = text.get_rect(centerx = rect_popup.centerx, top = rect_popup.top + gap_popup_n_txt)
        rect_text1 = text1.get_rect(top = rect_text.bottom + gap_popup_n_txt, centerx = rect_popup.centerx)

        btn_playagain.update(0, 0, popup_btn_w, popup_btn_h)
        btn_back.update(0, 0, popup_btn_w, popup_btn_h)
        btn_playagain = btn_playagain.inflate(20, 0)
        btn_playagain.bottom, btn_playagain.left = rect_popup.bottom - gap_popup_n_btn, rect_popup.left + gap_popup_n_btn
        btn_back.bottom, btn_back.right = rect_popup.bottom - gap_popup_n_btn, rect_popup.right - gap_popup_n_btn
        
        picture = pygame.image.load(f"images/{winner}_king.png").convert_alpha()
        sze = btn_back.top - rect_text1.bottom - gap_popup_n_btn
        picture = pygame.transform.scale(picture, (sze,sze))
        picture_rect = picture.get_rect(centerx = rect_popup.centerx, centery = (btn_back.top + rect_text1.bottom)//2)

        rect_text_playagain = text_playagain.get_rect(center = btn_playagain.center)
        rect_text_back = text_back.get_rect(center = btn_back.center)

        def draw_button(base_color, btn):
            is_hovered = btn.collidepoint(mouse_pos)
            color = tuple(c + 15 for c in base_color) if is_hovered else base_color
            
            pygame.draw.rect(screen, tuple(c - 30 for c in base_color), btn.copy().move(3,3), border_radius=5)
            pygame.draw.rect(screen, color, btn, border_radius=5)

        pygame.draw.rect(screen, popup_border_color, rect_popup_shadow, border_radius=5)
        pygame.draw.rect(screen, popup_color, rect_popup, border_radius=5)
        screen.blit(text, rect_text)
        screen.blit(text1, rect_text1)

        screen.blit(picture, picture_rect)

        draw_button(popup_btn_color, btn_playagain)
        draw_button(popup_btn_color, btn_back)

        screen.blit(text_playagain, rect_text_playagain)
        screen.blit(text_back, rect_text_back)

        return (btn_back, btn_playagain)

    (btn_back, btn_playagain) = render_popup(popup_y, btn_back, btn_playagain)

    if popup_y > popup_end_y:
        popup_y -= speed
    else:
        popup_y = popup_end_y

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            exit()
        if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            mouse_x, mouse_y = pygame.mouse.get_pos()
            print(btn_back, btn_playagain)
            if btn_back and btn_playagain:
                if btn_back.collidepoint(mouse_x, mouse_y) or btn_playagain.collidepoint(mouse_x, mouse_y):
                    click = True if btn_playagain.collidepoint(mouse_x, mouse_y) else False
                    while popup_y < popup_start_y:
                        popup_y += 20
                        (btn_back, btn_playagain) = render_popup(popup_y, btn_back, btn_playagain)
                    popup_y = popup_start_y
                    board.game_over = False
                    if click:
                        board.reset_game()
                    else:
                        return True
    return False

def main():
    pygame.init()
    screen = pygame.display.set_mode((screen_w,screen_h), pygame.SCALED)
    pygame.display.set_caption("Chess Game")
    clock = pygame.time.Clock()

    mode = draw_menu(screen)

    font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    font1 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size + 10)

    if mode is None:
        return

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

    x_start_btn = 760
    y_start_btn = 200
    btn_color = (96, 132, 188)
    icon_size = (26, 26)
    offset_btn = 80
    offset_txt = 16
    btn_shadow = 5
    gap_turn_n_btn = 160
    
    txt_new = font.render("New Game", True, btn_text_color)
    txt_undo = font.render("Undo", True, btn_text_color)
    txt_reset = font.render("Reset", True, btn_text_color)

    # Thiết lập các nút điều khiển
    icon_new = pygame.transform.scale(pygame.image.load("assets/new_game.png"), icon_size)
    icon_undo = pygame.transform.scale(pygame.image.load("assets/undo.png"), icon_size)
    icon_reset = pygame.transform.scale(pygame.image.load("assets/reset.png"), icon_size)

    btn_new = pygame.Rect(x_start_btn, y_start_btn, btn_w, btn_h)
    btn_undo = pygame.Rect(x_start_btn, y_start_btn + offset_btn, btn_w, btn_h)
    btn_reset = pygame.Rect(x_start_btn, y_start_btn + 2*offset_btn, btn_w, btn_h)

    rect_new = icon_new.get_rect(centery = btn_new.centery, left = btn_new.left + offset_txt)
    rect_undo = icon_undo.get_rect(centery = btn_undo.centery, left = btn_undo.left + offset_txt)
    rect_reset = icon_reset.get_rect(centery = btn_reset.centery, left = btn_reset.left + offset_txt)

    # Thiết lập viền bàn cờ
    board_border = pygame.image.load("images/board_border.png")
    board_border = pygame.transform.scale(board_border, board_size)
    board_border_rect = board_border.get_rect(topleft = (0,0))

    running = True
    back_clicked = False

    while running:
        #background and buttons
        screen.fill(background_color) 
        mouse_pos = pygame.mouse.get_pos()
        #print(mouse_pos)
        def draw_button(btn, txt, icon, rect):
            color = tuple(c + 20 for c in btn_color) if is_hovered(btn, mouse_pos) else btn_color
            pygame.draw.rect(screen, tuple(c - 20 for c in btn_color), btn.move(btn_shadow,btn_shadow), border_radius = 10)
            pygame.draw.rect(screen, color, btn, border_radius = 10)
            screen.blit(txt, txt.get_rect(centery = btn.centery, left = btn.left + 2 * offset_txt + icon_size[0]))
            screen.blit(icon, rect)
        draw_button(btn_new, txt_new, icon_new, rect_new)
        draw_button(btn_undo, txt_undo, icon_undo, rect_undo)
        draw_button(btn_reset, txt_reset, icon_reset, rect_reset)

        #board
        screen.blit(board_border, board_border_rect) 
        board.draw_board(offset_x = offset_x_board, offset_y = offset_y_board) 
        board.highlight_squares(offset_x = offset_x_board, offset_y = offset_y_board)
        board.draw_pieces(offset_x = offset_x_piece, offset_y = offset_y_piece)

        # Hiển thị thông báo kết quả khi game kết thúc
        string = "White To Move" if board.is_white_turn() else "Black To Move"
        if board.game_over:
            back_clicked = result_popup(screen, board)
        if board.game_over or back_clicked:
            string = "Well Played"
        txt_turn = font1.render(string, True, btn_color) 
        screen.blit(txt_turn, txt_turn.get_rect(centerx = btn_undo.centerx, bottom = btn_undo.top - gap_turn_n_btn))

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mouse_x, mouse_y = pygame.mouse.get_pos()

                # Xử lý sự kiện nhấn vào ô cờ
                if back_clicked == False and board.game_over == False and mouse_in_board(mouse_x, mouse_y):
                    row = (mouse_y - offset_y_board) // board.cell_size
                    col = (mouse_x - offset_x_board) // board.cell_size
                    board.handle_click(row, col)
                # Xử lý sự kiện nhấn nút
                # 1. game_over = true
                # 2. mouse not in board
                elif btn_new.collidepoint(mouse_x, mouse_y):
                    mode = draw_menu(screen)
                    back_clicked = False
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
                    popup_y = popup_start_y
                elif btn_undo.collidepoint(mouse_x, mouse_y):
                    board.undo_move()  # Hoàn tác nước đi
                    back_clicked = False
                    popup_y = popup_start_y
                elif btn_reset.collidepoint(mouse_x, mouse_y):
                    board.reset_game()  # Đặt lại trò chơi
                    back_clicked = False
                    popup_y = popup_start_y

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
