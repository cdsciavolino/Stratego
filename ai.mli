(* The [AI] module represents the computer opponent to the stratego user.
 * An AI chooses a move based on the board and its best interests *)
module type AI = sig

    (* See specs in board.mli for details *)
    type board = Board.GameBoard.t
    type victory = Board.GameBoard.victory
    type piece = Board.GameBoard.piece

    (**
     * [choose_best_board] takes in a board and returns a triple containing:
     * 1) The victory variant (which contains a board or a bool depending on if 
     *    the victor has been determined) that results from the AI making its 
     *    move.
     * 2) A piece list containing any pieces that were captured in the course
     *    of the AI's move.
     * 3) A string containing any messages resulting from the AI's move that
     *    need to be displayed to the user.
     *)
    val choose_best_board : board -> (victory * piece list * string)

end

module GameAI : AI