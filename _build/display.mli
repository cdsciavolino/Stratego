(* [Display] handles the task of showing the player the board and interacting
 * with the user visually through the user interface *)
module type Display = sig

    (* See board.mli for detailed information about board and piece *)
    type board = Board.GameBoard.t
    type piece = Board.GameBoard.piece

    (**
     * [print_message] takes in a string message to the user and displays it
     * followed by a line break.
     *)
    val print_message : string -> unit

    (**
     * [print_str] takes in a string message to the user and displays it
     *)
    val print_str : string -> unit

    (**
     * [display_board] displays the current board to the player
     *)
    val display_board : board -> unit

    (**
     * [print_list] displays a list of pieces to the player
     *)
    val print_list : piece list -> unit

    (**
     * [display_table] displays a reference table of the pieces to the player
     *)
    val display_table : unit -> unit

    (**
     * [display_rules] prints the rules to the user.
     *)
    val display_rules : unit -> unit

end

(* [TextDisplay] is a module that displays the board via ASCII characters in the
 * terminal *)
module TextDisplay : Display
