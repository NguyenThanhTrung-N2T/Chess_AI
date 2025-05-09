import random
import chess

class ChessAI:
    def __init__(self, level=1):
        self.level = level  # 1: dễ, 2: trung bình, 3: khó
        self.piece_values = {
            chess.PAWN: 100,
            chess.KNIGHT: 320,
            chess.BISHOP: 330,
            chess.ROOK: 500,
            chess.QUEEN: 900,
            chess.KING: 20000
        }

    def evaluate_board(self, board):
        value = 0
        for square in chess.SQUARES:
            piece = board.piece_at(square)
            if piece:
                val = self.piece_values[piece.piece_type]
                value += val if piece.color == chess.WHITE else -val
        if board.is_check():
            value += -50 if board.turn == chess.WHITE else 50
        return value

    def evaluate_move_safety(self, board, move):
        target_square = move.to_square
        attackers = board.attackers(not board.turn, target_square)  # Đã tối ưu hóa
        return not attackers

    def get_capture_value(self, board, move):
        captured_piece = board.piece_at(move.to_square)
        if captured_piece:
            return self.piece_values[captured_piece.piece_type]
        return 0

    def order_moves(self, board, moves):
        def move_score(move):
            score = 0
            if board.gives_check(move):
                score += 1000
            if board.is_capture(move):
                score += 800 + self.get_capture_value(board, move) # Ưu tiên bắt quân giá trị cao
            if self.evaluate_move_safety(board, move):
                score += 400
            score += self.evaluate_board(board)  # Lợi thế về quân
            return score

        return sorted(moves, key=move_score, reverse=board.turn == chess.WHITE)

    def minimax(self, board, depth, alpha, beta, maximizing):
        if depth == 0 or board.is_game_over():
            return self.evaluate_board(board)

        best_eval = float('-inf') if maximizing else float('inf')
        ordered_moves = self.order_moves(board, list(board.legal_moves)) # Sắp xếp thứ tự các nước đi

        for move in ordered_moves:
            board.push(move)
            eval = self.minimax(board, depth - 1, alpha, beta, not maximizing)
            board.pop()

            if maximizing:
                best_eval = max(best_eval, eval)
                alpha = max(alpha, eval)
                if beta <= alpha:
                    break
            else:
                best_eval = min(best_eval, eval)
                beta = min(beta, eval)
                if beta <= alpha:
                    break
        return best_eval

    def select_move(self, board):
        legal_moves = list(board.legal_moves)
        if not legal_moves:
            return None

        if self.level == 1:  # DỄ
            checks = [m for m in legal_moves if board.gives_check(m)]
            safe_moves = []
            for move in legal_moves:
                board.push(move)
                if not board.is_check():
                    safe_moves.append(move)
                board.pop()
            return random.choice(checks or safe_moves or legal_moves)

        elif self.level == 2:  # TRUNG BÌNH
            ordered_moves = self.order_moves(board, legal_moves)
            return ordered_moves[0] # Chọn nước đi đầu tiên sau khi sắp xếp

        elif self.level == 3:  # KHÓ (Minimax với tối ưu hóa)
            ordered_moves = self.order_moves(board, legal_moves)
            best_move = None
            best_eval = float('-inf') if board.turn == chess.WHITE else float('inf')

            for move in ordered_moves:
                board.push(move)
                eval = self.minimax(board, 3, float('-inf'), float('inf'), board.turn == chess.BLACK) # Độ sâu cố định. Tìm kiếm sâu dần và tĩnh lặng phức tạp hơn
                board.pop()

                if board.turn == chess.WHITE:
                    if eval > best_eval:
                        best_eval = eval
                        best_move = move
                else:
                    if eval < best_eval:
                        best_eval = eval
                        best_move = move
            return best_move