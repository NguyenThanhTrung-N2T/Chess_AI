import tkinter as tk
import os
from PIL import Image, ImageTk
from BanCo import BanCo
from tkinter import ttk

class Main:
    def __init__(self, root):
        self.root = root
        root.resizable(False,False)
        self.root.title("Chess AI")
        self.style = ttk.Style(root)
        self.style.theme_use('clam')
        self.images = {}  # Dùng để lưu hình ảnh quân cờ

        # Tải icon trước khi tạo button
        self.icon_nguoi = ImageTk.PhotoImage(Image.open("assets/nguoi_vs_nguoi.png").resize((24, 24)))
        self.icon_aide = ImageTk.PhotoImage(Image.open("assets/ai_de.png").resize((24, 24)))
        self.icon_aitb = ImageTk.PhotoImage(Image.open("assets/ai_tb.png").resize((24, 24)))
        self.icon_aikho = ImageTk.PhotoImage(Image.open("assets/ai_kho.png").resize((24, 24)))
        self.icon_undo = ImageTk.PhotoImage(Image.open("assets/undo.png").resize((24, 24)))
        self.icon_reset = ImageTk.PhotoImage(Image.open("assets/reset.png").resize((24, 24)))

        self.create_widgets()
        self.banco = BanCo(self.canvas)  # Vẽ bàn cờ trước
        self.draw_initial_position()     # Sau đó mới vẽ quân cờ
        # Style tùy chỉnh cho các nút
        button_style = ttk.Style()
        button_style.theme_use('default')
        button_style.configure('NguoiVsNguoi.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#69ef6c',
                            foreground='black')
        button_style.configure('AIDe.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#e0ffff',
                            foreground='black')
        button_style.configure('AITrungBinh.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#fffacd',
                            foreground='black')
        button_style.configure('AIKho.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#ef8080',
                            foreground='black')
        button_style.configure('Undo.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#e480ef',
                            foreground='black')
        button_style.configure('ResetVan.TButton',
                            padding=8,
                            font=('Segoe UI Emoji', 10),
                            anchor='center',
                            background='#c8ef80',
                            foreground='black')


    def create_widgets(self):
        main_frame = ttk.Frame(self.root, padding=10)
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))

        # Vùng bàn cờ
        chessboard_frame = ttk.Frame(main_frame, width=400, height=400, relief='sunken')
        self.canvas = tk.Canvas(chessboard_frame, width=400, height=400, bg='white', highlightthickness=0)
        self.canvas.pack()
        chessboard_frame.grid(row=0, column=0, padx=10, pady=10)

        # Khung điều khiển
        control_panel_frame = ttk.Frame(main_frame, padding=10)
        control_panel_frame.grid(row=0, column=1, sticky=(tk.N, tk.S))

        # Các nút chọn chế độ
        ttk.Label(control_panel_frame, text="Chế độ chơi:", font=('Arial', 12, 'bold')).pack(pady=(0, 5), anchor='w')

        button_style = ttk.Style()
        button_style.configure('TButton', padding=8, font=('Arial', 10), anchor='center')
        ttk.Button(control_panel_frame, text="Người vs Người",image=self.icon_nguoi, compound='left',style="NguoiVsNguoi.TButton").pack(fill='x', pady=2)
        ttk.Button(control_panel_frame, text="AI (Dễ)",image=self.icon_aide, compound='left',style="AIDe.TButton").pack(fill='x', pady=2)
        ttk.Button(control_panel_frame, text="AI (Trung Bình)",image=self.icon_aitb, compound='left',style="AITrungBinh.TButton").pack(fill='x', pady=2)
        ttk.Button(control_panel_frame, text="AI (Khó)",image=self.icon_aikho, compound='left',style="AIKho.TButton").pack(fill='x', pady=2)

        # Separator
        ttk.Separator(control_panel_frame, orient='horizontal').pack(fill='x', pady=5)

        ttk.Button(control_panel_frame, text="Undo",image=self.icon_undo, compound='left',style="Undo.TButton").pack(fill='x', pady=2)
        ttk.Button(control_panel_frame, text="Reset Ván",image=self.icon_reset, compound='left',style="ResetVan.TButton").pack(fill='x', pady=2)

        # Trạng thái
        status_frame = ttk.Frame(control_panel_frame, padding=5)
        status_frame.pack(fill='x', pady=(10, 0), anchor='s')
        ttk.Label(status_frame, text="🔊", font=('Arial', 12)).pack(side='left')
        ttk.Label(status_frame, text="Trạng thái: Chưa bắt đầu !", font=('Arial', 10, 'bold')).pack(side='left', padx=5)

        # Tùy chỉnh co giãn
        main_frame.grid_columnconfigure(0, weight=1)
        main_frame.grid_rowconfigure(0, weight=1)
        self.root.grid_columnconfigure(0, weight=1)
        self.root.grid_rowconfigure(0, weight=1)

    def load_piece_images(self):
        pieces = ['pawn', 'knight', 'bishop', 'rook', 'queen', 'king']
        colors = ['w', 'b']
        base_dir = os.path.dirname(__file__)

        for color in colors:
            for piece in pieces:
                filename = f"{color}_{piece}.png"
                path = os.path.join(base_dir, 'images', filename)
                if os.path.exists(path):
                    try:
                        img = Image.open(path).resize((50, 50), Image.Resampling.LANCZOS)
                        self.images[f"{color}_{piece}"] = ImageTk.PhotoImage(img)
                    except Exception as e:
                        print(f"Lỗi khi mở ảnh {filename}: {e}")
                        self.images[f"{color}_{piece}"] = None
                else:
                    print(f"Không tìm thấy: {path}")
                    self.images[f"{color}_{piece}"] = None

    def draw_initial_position(self):
        self.load_piece_images()  # Load hình ảnh trước
        size = 50
        setup = [
            ['b_rook', 'b_knight', 'b_bishop', 'b_queen', 'b_king', 'b_bishop', 'b_knight', 'b_rook'],
            ['b_pawn'] * 8,
            [''] * 8,
            [''] * 8,
            [''] * 8,
            [''] * 8,
            ['w_pawn'] * 8,
            ['w_rook', 'w_knight', 'w_bishop', 'w_queen', 'w_king', 'w_bishop', 'w_knight', 'w_rook'],
        ]

        for row in range(8):
            for col in range(8):
                piece_name = setup[row][col]
                if piece_name:
                    x = col * size + size // 2
                    y = row * size + size // 2
                    img = self.images.get(piece_name)
                    if img:
                        self.canvas.create_image(x, y, image=img, anchor='center')

if __name__ == "__main__":
    root = tk.Tk()
    app = Main(root)
    root.mainloop()
