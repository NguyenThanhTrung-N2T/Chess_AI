class Move:
    def __init__(self, from_pos, to_pos, piece, captured=None, had_moved=False, 
                 is_castling=False, is_en_passant=False, promotion=None):
        """
        Khởi tạo một đối tượng nước đi cho cờ vua.
        
        :param from_pos: Vị trí ban đầu của quân cờ (ví dụ: 'e2')
        :param to_pos: Vị trí đích mà quân cờ di chuyển đến (ví dụ: 'e4')
        :param piece: Loại quân cờ đang di chuyển (ví dụ: 'Pawn', 'Queen', ...)
        :param captured: Quân bị bắt (nếu có)
        :param had_moved: Đánh dấu nếu quân cờ này đã từng di chuyển
        :param is_castling: Đánh dấu nếu nước đi này là nhập thành
        :param is_en_passant: Đánh dấu nếu nước đi này là bắt tốt qua đường
        :param promotion: Nếu quân cờ là tốt và phong cấp, tham số này sẽ xác định quân phong cấp (ví dụ: 'Queen')
        """
        self.from_pos = from_pos       # Vị trí ban đầu của quân cờ
        self.to_pos = to_pos           # Vị trí đích
        self.piece = piece             # Loại quân cờ (Pawn, Queen, Rook, ...)
        self.captured = captured       # Quân bị bắt (nếu có)
        self.had_moved = had_moved     # Quân cờ đã di chuyển hay chưa
        self.is_castling = is_castling # Nếu đây là nhập thành
        self.is_en_passant = is_en_passant # Nếu đây là bắt tốt qua đường
        self.promotion = promotion     # Nếu phong cấp, loại quân mới (Queen, Rook, ...)

    def __repr__(self):
        return f"Move({self.from_pos} -> {self.to_pos}, Piece: {self.piece})"
