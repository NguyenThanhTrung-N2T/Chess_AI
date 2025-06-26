class ChessAI:
    def __init__(self, prolog_engine, level=1):
        self.prolog = prolog_engine # Tham chiếu đến Prolog engine đã được khởi tạo và consult
        self.level = level  # 1: dễ, 2: trung bình, 3: khó


    def select_move(self, current_player_color_str):
        """
        Chọn nước đi cho AI dựa vào Prolog.
        current_player_color_str: 'white' hoặc 'black'
        Trả về một tuple (Piece, C1, R1, C2, R2) hoặc None nếu không có nước đi.
        """
        level_str = ""
        if self.level == 1:
            level_str = "easy"
        elif self.level == 2:
            level_str = "medium"
        else: # self.level == 3
            level_str = "hard"

        # find_best_move(Color, Level, Piece, C1, R1, C2, R2) đã được định nghĩa trong Chess_AI.pl
        query = f"find_best_move({current_player_color_str}, {level_str}, Piece, C1, R1, C2, R2)."
        
        print(f"AI Query to Prolog: {query}") 
        
        try:
            solutions = list(self.prolog.query(query))
            if solutions:
                solution = solutions[0] 
                piece = solution['Piece'] # Đây là một atom từ Prolog, ví dụ: queen, pawn
                c1, r1, c2, r2 = solution['C1'], solution['R1'], solution['C2'], solution['R2']
                print(f"AI selected move: {piece} from ({c1},{r1}) to ({c2},{r2})")
                return (str(piece), c1, r1, c2, r2) 
            else:
                print("AI found no move from Prolog.")
                return None 
        except Exception as e:
            print(f"Error querying Prolog for AI move: {e}")
            return None