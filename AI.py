import chess
import random

PIECE_VALUES = {
    chess.PAWN: 100,
    chess.KNIGHT: 320,
    chess.BISHOP: 330,
    chess.ROOK: 500,
    chess.QUEEN: 900,
    chess.KING: 20000
}

CENTER_SQUARES = [chess.D4, chess.D5, chess.E4, chess.E5]

class AI:
    def __init__(self, level=2):  # 0 = dễ, 1 = trung bình, 2 = khó
        self.level = level

    def select_move(self, board):
        if self.level == 0:
            return self.random_move(board)
        elif self.level == 1:
            return self.minimax_root(board, depth=2, alpha_beta=True)
        else:
            return self.minimax_root(board, depth=4, alpha_beta=True)

    def random_move(self, board):
        legal_moves = list(board.legal_moves)
        safe_moves = [move for move in legal_moves if not self.would_be_in_check(board, move)]
        return random.choice(safe_moves or legal_moves)

    def would_be_in_check(self, board, move):
        board.push(move)
        is_in_check = board.is_check()
        board.pop()
        return is_in_check

    def minimax_root(self, board, depth, alpha_beta):
        best_move = None
        best_score = float('-inf') if board.turn == chess.WHITE else float('inf')

        for move in board.legal_moves:
            board.push(move)
            score = self.minimax(board, depth - 1, float('-inf'), float('inf'), not board.turn, alpha_beta)
            board.pop()

            if board.turn == chess.WHITE and score > best_score:
                best_score = score
                best_move = move
            elif board.turn == chess.BLACK and score < best_score:
                best_score = score
                best_move = move

        return best_move

    def minimax(self, board, depth, alpha, beta, is_maximizing, alpha_beta):
        if board.is_checkmate():
            return -99999 if is_maximizing else 99999
        if board.is_stalemate() or board.is_insufficient_material():
            return 0
        if depth == 0:
            return self.evaluate_board(board)

        if is_maximizing:
            max_eval = float('-inf')
            for move in board.legal_moves:
                board.push(move)
                eval = self.minimax(board, depth - 1, alpha, beta, False, alpha_beta)
                board.pop()
                max_eval = max(max_eval, eval)
                if alpha_beta:
                    alpha = max(alpha, eval)
                    if beta <= alpha:
                        break
            return max_eval
        else:
            min_eval = float('inf')
            for move in board.legal_moves:
                board.push(move)
                eval = self.minimax(board, depth - 1, alpha, beta, True, alpha_beta)
                board.pop()
                min_eval = min(min_eval, eval)
                if alpha_beta:
                    beta = min(beta, eval)
                    if beta <= alpha:
                        break
            return min_eval

    def evaluate_board(self, board):
        score = 0

        for square in chess.SQUARES:
            piece = board.piece_at(square)
            if piece:
                value = PIECE_VALUES.get(piece.piece_type, 0)
                if piece.color == chess.WHITE:
                    score += value
                else:
                    score -= value

                # Ưu tiên kiểm soát trung tâm
                if square in CENTER_SQUARES:
                    if piece.color == chess.WHITE:
                        score += 20
                    else:
                        score -= 20

        # Trừ điểm nếu vua bị chiếu
        if board.is_check():
            if board.turn == chess.WHITE:
                score -= 50
            else:
                score += 50

        return score
