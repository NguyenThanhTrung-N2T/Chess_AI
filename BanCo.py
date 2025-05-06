import tkinter as tk

class BanCo:
    def __init__(self, canvas, size=50):
        self.canvas = canvas
        self.size = size
        self.board_size = 8
        self.colors = ['#f0d9b5', '#b58863']  # màu sáng và tối xen kẽ
        self.kiem_tra_canvas()
        self.ve_ban_co()

    def kiem_tra_canvas(self):
        if self.canvas is None or not isinstance(self.canvas, tk.Canvas):
            raise ValueError("Canvas không hợp lệ hoặc chưa khởi tạo!")

    def ve_ban_co(self):
        self.canvas.delete("all")  # Xóa các phần tử cũ
        for row in range(self.board_size):
            for col in range(self.board_size):
                x1 = col * self.size
                y1 = row * self.size
                x2 = x1 + self.size
                y2 = y1 + self.size
                color = self.colors[(row + col) % 2]
                self.canvas.create_rectangle(x1, y1, x2, y2, fill=color, outline='')
