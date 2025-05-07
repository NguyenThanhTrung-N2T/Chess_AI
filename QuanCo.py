class Piece:
    def __init__(self, loai, mau):
        self.loai = loai    # pawn, rook, knight, bishop, queen, king
        self.mau = mau      # white hoặc black
        self.da_di = False  # đã từng di chuyển (phục vụ nhập thành, en passant)
        
    def __repr__(self):
        return f"Piece(loai={self.loai}, mau={self.mau}, da_di={self.da_di})"
