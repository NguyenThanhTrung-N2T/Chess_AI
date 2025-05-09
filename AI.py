import random
import chess

class ChessAI:
    def __init__(self, level=1):
        self.level = level  # 1: dễ, 2: trung bình, 3: khó

    def evaluate_board(self, board):
        piece_values = {
            chess.PAWN: 100,
            chess.KNIGHT: 320,
            chess.BISHOP: 330,
            chess.ROOK: 500,
            chess.QUEEN: 900,
            chess.KING: 20000
        }

        value = 0
        for square in chess.SQUARES:
            piece = board.piece_at(square)
            if piece:
                val = piece_values[piece.piece_type]
                value += val if piece.color == chess.WHITE else -val

        # Thưởng/phạt nếu đang bị chiếu
        if board.is_check():
            value += -50 if board.turn == chess.WHITE else 50

        return value

    def evaluate_move_safety(self, board, move):
        board.push(move)
        target_square = move.to_square
        attackers = board.attackers(not board.turn, target_square)
        board.pop()
        return len(attackers) == 0  # True nếu không bị tấn công sau khi đi

    def minimax(self, board, depth, alpha, beta, maximizing):
        if depth == 0 or board.is_game_over():
            return self.evaluate_board(board)

        best_eval = float('-inf') if maximizing else float('inf')

        for move in board.legal_moves:
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

        # EASY MODE
        if self.level == 1:
            checks = [m for m in legal_moves if board.gives_check(m)]
            safe_moves = []
            for move in legal_moves:
                board.push(move)
                if not board.is_check():
                    safe_moves.append(move)
                board.pop()
            return random.choice(checks or safe_moves or legal_moves)

        # NORMAL MODE
        elif self.level == 2:
            move_scores = []

            for move in legal_moves:
                gives_check = board.gives_check(move)
                is_capture = board.is_capture(move)
                is_safe = self.evaluate_move_safety(board, move)

                board.push(move)
                score = self.evaluate_board(board)

                if board.is_checkmate():
                    score += 10000
                elif board.is_check():
                    score += 80
                elif is_capture:
                    score += 60
                elif gives_check:
                    score += 40

                if not is_safe:
                    score -= 100  # phạt nếu nước đi dẫn đến quân bị bắt
                else:
                    score += 30   # thưởng nếu nước đi an toàn

                if board.is_repetition(2):
                    score -= 100

                board.pop()
                move_scores.append((move, score))

            move_scores.sort(key=lambda x: x[1], reverse=board.turn == chess.WHITE)
            top_moves = move_scores[:min(5, len(move_scores))]
            chosen = random.choice([m[0] for m in top_moves])
            return chosen

        # HARD MODE
        elif self.level == 3:
            move_evals = []

            for move in legal_moves:
                is_safe = self.evaluate_move_safety(board, move)
                board.push(move)
                eval = self.minimax(board, 3, float('-inf'), float('inf'), board.turn)
                board.pop()
                if not is_safe:
                    eval -= 150  # Phạt nặng nếu di chuyển vào ô bị tấn công
                else:
                    eval += 50   # Thưởng nếu đi vào ô an toàn
                move_evals.append((move, eval))

            move_evals.sort(key=lambda x: x[1], reverse=board.turn == chess.WHITE)
            top_moves = move_evals[:min(3, len(move_evals))]
            chosen = random.choice([m[0] for m in top_moves])
            return chosen
