(* [board] represents the type of a Board *)
type board = Board.GameBoard.t

(**
 * [manual_setup] creates the board that exists at the start of gameplay; it
 * allows the user to choose the start position of each of his/her pieces.
 * Returns a board with both the user's pieces and the AI's pieces.
 *)
val manual_setup : unit -> board

(**
 * [play] handles the gameplay of stratego. It takes in a board, prompts the
 * player to input a move, checks that the move is valid, executes that move,
 * prompts the AI to make a move, executes that move, and then returns the final
 * board.
 *)
val play : board -> board

(**
 * [auto_setup] is a newly instantiated board with all of the AI's and player's
 * pieces on it, ready to go.
 *)
val auto_setup : unit -> board
