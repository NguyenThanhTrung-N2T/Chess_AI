import pygame
import os
from BanCo import Board  # Import lớp Board của bạn
from AI import ChessAI  # Import lớp AI của bạn
from pyswip import Prolog

prolog = Prolog()
# Nạp tất cả các file Prolog cần thiết. Thứ tự có thể quan trọng.
prolog.consult("Chess_Helper.pl")  # Nếu Chess_Law.pl hoặc Chess_AI.pl dùng vị từ từ đây
prolog.consult("Chess_Law.pl")
prolog.consult("Chess_AI.pl")    # Để AI có thể hoạt động

screen_w = 1000
screen_h = 700

button_text_size = 16
subtitle_text_size = 18
title_text_size = 40
gap_screen_n_title = 60
gap_title_n_subtitle = 70
gap_btn_n_btn = 30
title_text_color = (80, 84, 24)
subtitle_text_color = (80, 84, 24)

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

click_sound = pygame.mixer.Sound("sounds/click.MP3")

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
        background_image = pygame.image.load("assets/background.png")
        background_image = pygame.transform.scale(background_image, (screen_w,screen_h))
    except pygame.error as e:
        print(f"Error loading image: {e}")
        return None

    clock = pygame.time.Clock()
    button_font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    title_font = pygame.font.Font("fonts/pixelmix_bold.ttf", title_text_size)
    subtitle_font = pygame.font.Font("fonts/pixelmix_bold.ttf", subtitle_text_size)
    icon_size = (40, 40)

    # Title
    title_text = title_font.render("PIXEL CHESS GAME", False, title_text_color)
    subtitle_text = subtitle_font.render("Choose a game mode", False, subtitle_text_color)
    rect_title_text = title_text.get_rect(centerx = screen_w//2, top = gap_screen_n_title)
    rect_subtitle_text = subtitle_text.get_rect(centerx = screen_w//2, top = rect_title_text.bottom + gap_title_n_subtitle )

    # Load icon
    icon_pvp = pygame.transform.scale(pygame.image.load("assets/nguoi_vs_nguoi.png"), icon_size)
    icon_ai_de = pygame.transform.scale(pygame.image.load("assets/ai_de.png"), icon_size)
    icon_ai_tb = pygame.transform.scale(pygame.image.load("assets/ai_tb.png"), icon_size)
    icon_ai_kho = pygame.transform.scale(pygame.image.load("assets/ai_kho.png"), icon_size)

    # Định nghĩa các button
    rect_btn = pygame.Rect(0, 0, 320, 60)
    rect_btn.centerx, rect_btn.top = screen_w // 2, rect_subtitle_text.bottom + gap_btn_n_btn
    rect_btn1 = rect_btn.copy()
    rect_btn2 = rect_btn.copy()
    rect_btn3 = rect_btn.copy()
    rect_btn1.top = rect_btn.bottom + gap_btn_n_btn
    rect_btn2.top = rect_btn1.bottom + gap_btn_n_btn
    rect_btn3.top = rect_btn2.bottom + gap_btn_n_btn

    # Sound
    game_start_sound = pygame.mixer.Sound("sounds/game_start.MP3")

    buttons = [
        {"rect": rect_btn, "text": "Player vs Player", "icon": icon_pvp,   "color": (74, 144, 226), "mode": "pvp"},
        {"rect": rect_btn1, "text": "Player vs AI (Easy)", "icon": icon_ai_de, "color": (80, 227, 194), "mode": "easy"},
        {"rect": rect_btn2, "text": "Player vs AI (Normal)", "icon": icon_ai_tb, "color": (245, 166, 35), "mode": "normal"},
        {"rect": rect_btn3, "text": "Player vs AI (Hard)", "icon": icon_ai_kho, "color": (208, 39, 30), "mode": "hard"},
    ]

    selected_mode = None
    running = True

    while running:
        screen.blit(background_image, (0, 0))
        mouse_pos   = pygame.mouse.get_pos()
        mouse_click = pygame.mouse.get_pressed()[0]

        screen.blit(title_text, rect_title_text)
        screen.blit(subtitle_text, rect_subtitle_text) 

        for btn in buttons:
            rect       = btn["rect"]
            base_color = btn["color"]
            hover_col  = tuple(min(255, c+30) for c in base_color)
            click_col  = tuple(max(0, c-30) for c in base_color)

            if is_hovered(rect, mouse_pos):
                color = click_col if mouse_click else hover_col
            else:
                color = base_color

            pygame.draw.rect(screen, tuple(c - 30 for c in color), rect.copy().move(4,4), border_radius=5)
            pygame.draw.rect(screen, color, rect, border_radius=5)
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
                        game_start_sound.play()
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
    winner = "b"
    if board.status_message.startswith("White"):
        winner = "w"
    elif board.status_message.startswith("Draw"):
        winner = "d"
    font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    font2 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size - 3)
    font3 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size + 6)
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

        if winner == "d":
            index = board.status_message.find("Draw by")
            text1 = font3.render("Draw", False, popup_text_color)
            text2 = font2.render("By", False, popup_text_color)
            text3 = font2.render(board.status_message[index+8:], False, popup_text_color)
            rect_text1 = text1.get_rect(top = rect_text.bottom + 5*gap_popup_n_txt, centerx = rect_popup.centerx)
            rect_text2 = text2.get_rect(top = rect_text1.bottom + gap_popup_n_txt, centerx = rect_popup.centerx)
            rect_text3 = text3.get_rect(top = rect_text2.bottom + gap_popup_n_txt, centerx = rect_popup.centerx)

        btn_playagain.update(0, 0, popup_btn_w, popup_btn_h)
        btn_back.update(0, 0, popup_btn_w, popup_btn_h)
        btn_playagain = btn_playagain.inflate(20, 0)
        btn_playagain.bottom, btn_playagain.left = rect_popup.bottom - gap_popup_n_btn, rect_popup.left + gap_popup_n_btn
        btn_back.bottom, btn_back.right = rect_popup.bottom - gap_popup_n_btn, rect_popup.right - gap_popup_n_btn
        
        if winner != "d":
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

        if winner == "d":
            screen.blit(text2, rect_text2)
            screen.blit(text3, rect_text3)
        else:
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
                        # Click SOUND
                        click_sound.play()
                        return True
    return False

# reset facts trong Prolog khi bắt đầu trò chơi mới
def reset_prolog():
    """Reset các fact động trong Prolog về trạng thái ban đầu."""
    list(prolog.query("retractall(last_move(_,_,_,_)), assertz(last_move(0,0,0,0))"))
    list(prolog.query("retractall(king_moved(_))"))
    list(prolog.query("retractall(rook_moved(_,_))")) # Đảm bảo arity đúng, ví dụ rook_moved/2
    list(prolog.query("retractall(halfmove_clock(_)), assertz(halfmove_clock(0))"))
    list(prolog.query("retractall(board_history(_)), assertz(board_history([]))")) # Đảm bảo arity đúng, ví dụ board_history/1
    # piece_at/4 sẽ được xử lý bởi board.assert_board_state() khi board mới được tạo.
    print("Dynamic Prolog facts (last_move, king_moved, etc.) reset by Main.py's reset_prolog function.")

def main():
    pygame.init()
    pygame.font.init()
    pygame.mixer.init()
    screen = pygame.display.set_mode((screen_w,screen_h), pygame.SCALED)
    icon = pygame.image.load("images/w_pawn.png")
    pygame.display.set_caption("Pixel Chess Game")
    pygame.display.set_icon(icon)
    clock = pygame.time.Clock()

    mode = draw_menu(screen)
    if mode is None:
        # User quit early; exit cleanly
        pygame.quit()
        return
    
    font = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size)
    font1 = pygame.font.Font("fonts/pixelmix_bold.ttf", button_text_size + 10)

    if mode is None:
        return

    board = Board(screen, cell_size, prolog_engine=prolog)  # Truyền prolog engine cho Board

    if mode == "easy":
        board.ai_level = 1
        board.play_with_ai = True
        board.ai = ChessAI(prolog_engine=prolog, level=1) # Truyền prolog engine cho AI
    elif mode == "normal":
        board.ai_level = 2
        board.play_with_ai = True
        board.ai = ChessAI(prolog_engine=prolog, level=2) # Truyền prolog engine cho AI
    elif mode == "hard":
        board.ai_level = 3
        board.play_with_ai = True
        board.ai = ChessAI(prolog_engine=prolog, level=3) # Truyền prolog engine cho AI
    else: # Chế độ PvP hoặc các chế độ không có AI
        board.play_with_ai = False
        board.ai = None

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

        # vẽ viền các ô mà quân cờ có thể di chuyển đến
        # board.highlight_squares(offset_x = offset_x_board, offset_y = offset_y_board)
        board.draw_pieces(offset_x = offset_x_piece, offset_y = offset_y_piece)

        # Hiển thị thông báo kết quả khi game kết thúc
        #string = "White To Move" if board.is_white_turn() else "Black To Move"
        # if board.game_over:
        #     back_clicked = result_popup(screen, board)
        # if board.game_over or back_clicked:
        #     string = "Well Played"
        # txt_turn = font1.render(string, True, btn_color) 
        # rect_txt_turn = txt_turn.get_rect(centerx = btn_undo.centerx, bottom = btn_undo.top - gap_turn_n_btn)
        # if is_hovered(rect_txt_turn, mouse_pos):
        #     txt_turn = font1.render(string, True, tuple(c+30 for c in btn_color)) 
        # screen.blit(txt_turn, rect_txt_turn)




        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mouse_x, mouse_y = pygame.mouse.get_pos()

                # Xử lý sự kiện nhấn vào ô cờ
                if back_clicked == False and board.game_over == False and mouse_in_board(mouse_x, mouse_y):
                    row = (mouse_y - offset_y_board) // board.cell_size
                    col = (mouse_x - offset_x_board) // board.cell_size
                    row_prolog = 8 - row  # Chuyển đổi từ hệ tọa độ của pygame sang hệ tọa độ của Prolog
                    col_prolog = col + 1  # Chuyển đổi từ hệ tọa độ của pygame sang hệ tọa độ của Prolog
                    board.handle_click(row_prolog, col_prolog)
                # Xử lý sự kiện nhấn nút
                # 1. game_over = true
                # 2. mouse not in board
                elif btn_new.collidepoint(mouse_x, mouse_y):
                    # Click SOUND
                    click_sound.play()
                    # reset Prolog facts
                    reset_prolog()

                    mode = draw_menu(screen)
                    if mode is None: # Nếu người dùng thoát khỏi menu
                        running = False # Kết thúc game loop chính
                        continue

                    back_clicked = False
                    # Reset lại bàn cờ với chế độ mới
                    board = Board(screen, cell_size, prolog_engine=prolog) # Tạo Board mới
                    if mode == "easy":
                        board.ai_level = 1
                        board.play_with_ai = True
                        board.ai = ChessAI(prolog_engine=prolog, level=1)
                    elif mode == "normal":
                        board.ai_level = 2
                        board.play_with_ai = True
                        board.ai = ChessAI(prolog_engine=prolog, level=2)
                    elif mode == "hard":
                        board.ai_level = 3
                        board.play_with_ai = True
                        board.ai = ChessAI(prolog_engine=prolog, level=3)
                    else: # Chế độ PvP
                        board.play_with_ai = False
                        board.ai = None
                    popup_y = popup_start_y
                elif btn_undo.collidepoint(mouse_x, mouse_y):
                    board.undo_move()  # Hoàn tác nước đi
                    back_clicked = False
                    popup_y = popup_start_y
                elif btn_reset.collidepoint(mouse_x, mouse_y):
                    # Click SOUND
                    click_sound.play()
                    # Gọi hàm reset của Board, hàm này sẽ tự xử lý việc reset Prolog facts
                    # và giữ nguyên chế độ chơi (ai_level, play_with_ai, ai object)
                    board.reset_game() 
                    # Không cần tạo lại đối tượng Board hay AI ở đây nữa,
                    # vì board.reset_game() sẽ xử lý nội bộ.
                    back_clicked = False
                    popup_y = popup_start_y

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
