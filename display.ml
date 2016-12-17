open Board.GameBoard

module type Display = sig
  type board = t
  type piece = Board.GameBoard.piece
  val print_message : string -> unit
  val print_str : string -> unit
  val display_board : board -> unit
  val print_list : piece list -> unit
  val display_table : unit -> unit
  val display_rules : unit -> unit
end

module TextDisplay : Display = struct

  (* See board.mli for detailed information about board and piece *)
  type board = Board.GameBoard.t
  type piece = Board.GameBoard.piece

  (*[print_message s] prints s to the console followd by a line break*)
  let print_message s = print_endline s

  (*[print_str s] prints s to the console*)
  let print_str s = print_string s

  (*[string_of_piece_ai p] returns the string to be displayed of the given
   *AI piece p*)
  let string_of_piece_ai p = match (get_rank p) with
  |x when x = 0 -> "B "
  |x when x = 1 -> "S "
  |x when x < 10 -> (string_of_int x)^" "
  |x when x = 11 -> "F "
  |x -> string_of_int x

  (*[string_of_piece_user p] returns the string to be displayed of the given
   *user piece p*)
  let string_of_piece_user p = match (get_rank p) with
  |x when x = 0 -> " B "
  |x when x = 1 -> " S "
  |x when x < 10 -> " "^(string_of_int x)^" "
  |x when x = 11 -> " F "
  |x -> (string_of_int x)^" "

  (*[print_row b r] prints the current row r of board b to the console.*)
  let print_row (board:board) (row:int) =
    for col = 0 to 9 do
      let pos = make_position col row in
      let p = (search pos board) in ();
      (match p with
      |None -> print_string "   ";
      |Some x ->
        if not (get_player x) then
            if (get_been_seen x) then
                print_string ("A"^(string_of_piece_ai x))
            else print_string "AI "
        else (print_string (string_of_piece_user x)));
      print_string "| "
    done;
    print_endline ""

  (*See display.mli*)
  let display_board (b:board) =
    print_endline "";
    print_endline "     _________________________________________________";
    for row = 9 downto 0 do
      print_endline "    |    |    |    |    |    |    |    |    |    |    | ";
      print_string  (" "^(string_of_int row)^"  | ");
      (print_row b row);
      print_endline "    |____|____|____|____|____|____|____|____|____|____|";
    done;
    print_endline "       0    1    2    3    4    5    6    7    8    9"

  (*See display.mli*)
  let display_rules () =
    print_endline
    "
    Stratego is a turn based strategy game. A player wins if they capture their
    opponent's flag or if their opponent can't make any moves.

    To capture the flag you use your army of 40 pieces.
    Pieces have a rank and represent individual officers and soldiers in an army.
    In addition to those ranked pieces you can use bombs to protect your flag.

    Pieces move 1 tile per turn, horizontally or vertically.
    Only the scout (rank 2) can move over multiple empty tiles per turn 
    (like a rook in chess).
    Pieces cannot jump over other pieces.

    If a piece is moved onto a tile that is occupied by an opposing piece,
    the weaker piece is removed from the board. If the attacking piece wins the
    battle, it occupies the tile formerly occupied by the defeated piece.

    If the engaging pieces are of equal rank, they are both removed.
    Pieces may not move onto a tile already occupied by another piece without 
    attacking.

    The exception to the rule of the higher rank winning is the spy.
    If a spy (rank 1) attacks the marshal (rank 10), the spy wins.
    However, if the marshal initiates the battle, then the spy does not
    have the element of surprise and is defeated.

    If an opponent piece attacks a bomb, the player is removed but the bomb 
    remains.
    Bombs are removed only when they are attacked by a miner (rank 3).

    The bombs and the flag cannot be moved.
    The flag can be captured by an opponent piece of any rank.
    When you capture the flag of your opponent you win the game.

    To move, type the (x,y) position of the piece you want to move as xy 
    followed by the (x,y) position of the tile you want to move to in the same 
    format i.e. to move a piece from (1,1) to (1,2), enter 11 12";
    print_endline
    "
    Once the game begins, the following commands will be available:
    \tTABLE - Displays a table linking the names of pieces to their ranks
    \tCAPTURED - Displays the pieces captured by each player
    \tRULES - Displays the rules and commands available
    \tQUIT - Exits the game"

  (*See display.mli*)
  let print_list (l:piece list) =
    print_string "Pieces: ";
    List.iter (fun x -> print_string ((string_of_piece_user x)^" | ")) l;
    print_endline ""

  (*See display.mli*)
  let display_table () =
    print_endline "   __________________";
    print_endline "  |                  |";
    print_endline "  |   F - Flag       |";
    print_endline "  |   B - Bomb       |";
    print_endline "  |   1 - Spy        |";
    print_endline "  |   2 - Scout      |";
    print_endline "  |   3 - Miner      |";
    print_endline "  |   4 - Sergeant   |";
    print_endline "  |   5 - Lieutenant |";
    print_endline "  |   6 - Captain    |";
    print_endline "  |   7 - Major      |";
    print_endline "  |   8 - Colonel    |";
    print_endline "  |   9 - General    |";
    print_endline "  |  10 - Marshall   |";
    print_endline "  |__________________|";
    print_endline ""

end