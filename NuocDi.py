class NuocDi:
    def __init__(self, from_pos, to_pos, piece, captured=None, is_castling=False, is_en_passant=False, promotion=None):
        self.from_pos = from_pos
        self.to_pos = to_pos
        self.piece = piece
        self.captured = captured
        self.is_castling = is_castling
        self.is_en_passant = is_en_passant
        self.promotion = promotion
