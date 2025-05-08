import pygame
import os
from BanCo import Board  # Import lớp Board của bạn
import chess

def main():
    pygame.init()
    screen = pygame.display.set_mode((1000, 800))
    pygame.display.set_caption("Chess Game")
    clock = pygame.time.Clock()

    # Khởi tạo màn hình và lớp Board
    board = Board(screen, 100)

    # Thiết lập phông chữ và màu sắc cho nút
    button_font = pygame.font.SysFont("Arial", 28, bold=True)
    button_color = (100, 150, 250)

    # Thiết lập các nút điều khiển
    icon_new = pygame.transform.scale(pygame.image.load("assets/new_game.png"), (40, 40))
    icon_undo = pygame.transform.scale(pygame.image.load("assets/undo.png"), (40, 40))
    icon_reset = pygame.transform.scale(pygame.image.load("assets/reset.png"), (40, 40))

    btn_new = pygame.Rect(810, 100, 180, 50)
    btn_undo = pygame.Rect(810, 180, 180, 50)
    btn_reset = pygame.Rect(810, 260, 180, 50)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mouse_x, mouse_y = pygame.mouse.get_pos()

                # Xử lý sự kiện nhấn vào ô cờ
                if mouse_x < 800:
                    row = mouse_y // board.cell_size
                    col = mouse_x // board.cell_size
                    board.handle_click(row, col)

                # Xử lý sự kiện nhấn nút
                elif btn_new.collidepoint(mouse_x, mouse_y):
                    board.reset_game()  # Đặt lại trò chơi mới

                elif btn_undo.collidepoint(mouse_x, mouse_y):
                    board.undo_move()  # Hoàn tác nước đi

                elif btn_reset.collidepoint(mouse_x, mouse_y):
                    board.reset_game()  # Đặt lại trò chơi

        # Vẽ bàn cờ và các quân cờ
        screen.fill((239, 249, 253))
        board.draw_board(offset_x=0, offset_y=0)
        board.draw_pieces(offset_x=0, offset_y=0)

        # Vẽ các nút chức năng
        pygame.draw.rect(screen, button_color, btn_new, border_radius=10)
        screen.blit(icon_new, (btn_new.x + 10, btn_new.y + 5))
        txt_new = button_font.render("New Game", True, (255, 255, 255))
        screen.blit(txt_new, (btn_new.x + 60, btn_new.y + (50 - txt_new.get_height()) // 2))

        pygame.draw.rect(screen, button_color, btn_undo, border_radius=10)
        screen.blit(icon_undo, (btn_undo.x + 10, btn_undo.y + 5))
        txt_undo = button_font.render("Undo", True, (255, 255, 255))
        rect_undo = txt_undo.get_rect(center=(btn_undo.centerx + 15, btn_undo.centery))
        screen.blit(txt_undo, rect_undo)

        pygame.draw.rect(screen, button_color, btn_reset, border_radius=10)
        screen.blit(icon_reset, (btn_reset.x + 10, btn_reset.y + 5))
        txt_reset = button_font.render("Reset", True, (255, 255, 255))
        rect_reset = txt_reset.get_rect(center=(btn_reset.centerx + 10, btn_reset.centery))
        screen.blit(txt_reset, rect_reset)

        # Hiển thị thông báo kết quả khi game kết thúc
        if board.game_over:
            # Phần 1: Kết thúc trò chơi
            game_over_message = ">> End Game <<"
            game_over_text = button_font.render(game_over_message, True, (255, 0, 0))
            screen.blit(game_over_text, (350, 350))  # Hiển thị "Kết thúc" ở giữa bàn cờ

            # Phần 2: Thông báo người thắng hoặc hòa
            result_message = board.status_message
            result_text = button_font.render(result_message, True, (255, 0, 0))
            screen.blit(result_text, (350, 390))  # Hiển thị kết quả thắng hoặc hòa ở dưới "Kết thúc"

        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
