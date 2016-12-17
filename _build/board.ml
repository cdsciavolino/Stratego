(* Tuple represents a tuple that has a compare function *)
module type Tuple = sig
    type t
    val compare : t -> t -> int
end

(* IntTuple is a Tuple that has type int * int and implemented compare *)
module IntTuple : (Tuple with type t = (int * int)) = struct

    type t = (int * int)

    (* [compare t1 t2] is a positive integer if t1 > t2, zero if t1 = t2, and
     * a negative integer if t1 < t2. *)
    let compare (t1:t) (t2:t) =
        match Pervasives.compare (fst t1) (fst t2) with
            | 0 -> Pervasives.compare (snd t1) (snd t2)
            | c -> c
end

module type Board = sig
    type position
    val make_position : int -> int -> position
    val get_tuple : position -> int * int

    type piece
    val user_pieces_lost : piece array
    val ai_pieces_lost : piece array
    val make_piece : int -> bool -> bool -> piece
    val get_rank : piece -> int
    val get_player : piece -> bool
    val get_been_seen : piece -> bool

    type t
    type victory = Active of t | Victory of bool
    val empty_board : unit -> t
    val search : position -> t -> (piece option)
    val add_mapping : position -> (piece option) -> t -> t
    val equal_board : t -> t -> bool
    val board_fold : (position -> piece option -> 'a -> 'a) -> t -> 'a -> 'a
    val board_iter : (position -> piece option -> unit) -> t -> unit
    val get_list_all_pieces: bool -> piece list
    val fill: t -> position list -> piece list -> position -> bool -> t
    val do_setup: t -> bool -> t
    val string_from_piece : piece -> string
    val get_possible_moves : t -> bool -> piece -> position -> position list
    val is_valid_move : t -> bool -> position -> position -> (bool * string)
    val make_move : t -> position -> position -> (victory * piece list * string)
end

(* BoardMap contains all the necessary functions to make changes to a Map.S,
 * the type of Map.Make(IntTuple) *)
module BoardMap = Map.Make(IntTuple)

module GameBoard : Board = struct
    (** 
     * AF: an (int * int) to piece option map represents a 10x10 stratego board
     *     where the int tuple represents an (x,y) coordinate on the board
     *     where (0,0) is in the bottom left corner tile and (9, 9) is the top 
     *     right corner tile.  The piece option is None if there is no piece on  
     *     tile (x, y) and Some p if p (: piece) is placed on the tile (x, y)
     *
     * RI: the (int*int) tuple represents (x,y) where 0 <= x,y < 10
     *     Some p (:piece) such that:
     *        - a max of 80 pieces are on the board
     *        - each player has a max of 40 pieces on the board
     *        - each player has at most the number of each rank as
     *          specified in the rules of Stratego
     *        - No piece will ever be mapped to a grid tile outside of the 
     *          board
     *        - Both players must still have a flag on the board for the game
     *          to be active
     *)

    (* See board.mli file *)
    type position = int * int

    (* See board.mli file *)
    let make_position (x:int) (y:int) : int * int = (x, y)

    (* See board.mli file *)
    let get_tuple (pos:position) : int * int = pos

    (* See board.mli file *)
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen : bool
    }

    (* See board.mli file *)
    let make_piece (r:int) (pla:bool) (seen:bool) : piece =
        { rank = r; player = pla; hasBeenSeen = seen }

    (* See board.mli file *)
    let get_rank (p:piece) : int = p.rank

    (* See board.mli file *)
    let get_player (p:piece) : bool = p.player

    (* See board.mli file *)
    let get_been_seen (p:piece) : bool = p.hasBeenSeen

    (* See board.mli file *)
    let user_pieces_lost = Array.make 40 (make_piece 12 true false)

    (* See board.mli file *)
    let ai_pieces_lost = Array.make 40 (make_piece 12 true false)

    (* See board.mli file *)
    type t = (piece option) BoardMap.t

    (* See board.mli file *)
    type victory = Active of t | Victory of bool

    (* Represents a direction that an iterator will step on a board *)
    type dir = N | E | S | W

    (* See board.mli file *)
    let search pos board = BoardMap.find pos board

    (* See board.mli file *)
    let add_mapping pos piece board = BoardMap.add pos piece board

    (* See board.mli file *)
    let equal_board b1 b2 = BoardMap.equal (=) b1 b2

    (* See board.mli file *)
    let board_fold f board acc = BoardMap.fold f board acc

    (* See board.mli file *)
    let board_iter f board = BoardMap.iter f board

    (**
     * [none_whole_board board pos] is [board] with all tiles in the 10x10 grid
     * filled with None starting from [pos]
     *)
    let rec none_whole_board board pos =
        match pos with
        |(10,9) -> board
        |(10,y) -> none_whole_board board (0, y+1)
        |(x,y) -> let brd = add_mapping (x,y) None board
                    in none_whole_board brd (x+1,y)

    (* See board.mli file *)
    let empty_board () = none_whole_board BoardMap.empty (0, 0)

    (**
     * [string_from_piece piece] is the name of [piece] given its rank.
     * Raises:
     *  - Failure when passed an invalid piece
     *)
    let string_from_piece piece =
        match piece.rank with
        | 0 -> "Bomb"
        | 1 -> "Spy"
        | 2 -> "Scout"
        | 3 -> "Miner"
        | 4 -> "Sergeant"
        | 5 -> "Lieutenant"
        | 6 -> "Captain"
        | 7 -> "Major"
        | 8 -> "Colonel"
        | 9 -> "General"
        | 10 -> "Marshal"
        | 11 -> "Flag"
        | _ -> failwith "Not a valid piece"

    (* See board.mli file *)
    let get_list_all_pieces player =
        let p       = {rank=0; player=player; hasBeenSeen=false} in
        let col     = {p with rank=8} in
        let major   = {p with rank=7} in
        let cap     = {p with rank=6} in
        let lieut   = {p with rank=5} in
        let serg    = {p with rank=4} in
        let miner   = {p with rank=3} in
        let scout   = {p with rank=2} in
        let bomb_list = [p; p; p; p; p; p] in
        let marsh_list = [{p with rank=10}] in
        let gen_list = [{p with rank=9}] in
        let col_list = [col; col] in
        let maj_list = [major; major; major] in
        let cap_list = [cap; cap; cap; cap] in
        let lieut_list = [lieut; lieut; lieut; lieut] in
        let serg_list = [serg; serg; serg; serg] in
        let mine_list = [miner; miner; miner; miner; miner] in
        let sco_list = [scout; scout; scout; scout; scout; scout; scout; 
                        scout] in
        let spy_list = [{p with rank=1}] in
        let flag_lst = [{p with rank=11}] in
        flag_lst @ bomb_list @ marsh_list @ gen_list @ col_list @ maj_list @
        cap_list @ lieut_list @ serg_list @ mine_list @ sco_list @ spy_list

    (**
     * [step board b pos dir] is the list of positions a scout can move to on
     * [board] given the player [b] from position [pos] in direction [dir].
     * This function steps until it meets an enemy piece (including that
     * position in the list), a member of its own team (excluding that
     * position from the list), or until a board edge is reached.
     *)
    let rec step board b pos dir =
        let (x, y) = pos in
        match dir with
        | N ->
            if y=9 then [] else
            (match (search (x, y+1) board) with
            | None -> (x, y+1)::(step board b (x, y+1) N)
            | Some piece ->
                if (piece.player = b) then [] else [(x, y+1)])
        | E ->
            if x=9 then [] else
            (match (search (x+1, y) board) with
            | None -> (x+1, y)::(step board b (x+1, y) E)
            | Some piece ->
                if (piece.player = b) then [] else [(x+1, y)])
        | S ->
            if y=0 then [] else
            (match (search (x, y-1) board) with
            | None -> (x, y-1)::(step board b (x, y-1) S)
            | Some piece ->
                if (piece.player = b) then [] else [(x, y-1)])
        | W ->
            if x=0 then [] else
            (match (search (x-1, y) board) with
            | None -> (x-1, y)::(step board b (x-1, y) W)
            | Some piece ->
                if (piece.player = b) then [] else [(x-1, y)])

    (**
     * [get_scout_moves board b pos] is the list of positions a scout can move
     * to in all directions by player [b] on board [board] from position [pos].
     *)
    let get_scout_moves board b pos =
        let up_list = step board b pos N in
        let right_list = step board b pos E in
        let bot_list = step board b pos S in
        let left_list = step board b pos W in
        up_list @ right_list @ bot_list @ left_list

    (* See board.mli file *)
    let get_possible_moves board b piece pos =
        match piece.rank with
        | 0 | 11 -> []
        | 2 -> get_scout_moves board b pos
        | _ ->
            let (x, y) = pos in
            let left = if x=0 then [] else [(x-1, y)] in
            let right = if x=9 then [] else [(x+1, y)] in
            let top = if y=9 then [] else [(x, y+1)] in
            let bot = if y=0 then [] else [(x, y-1)] in
            List.filter (fun a -> match search a board with
                                | Some p when p.player = piece.player -> false
                                |_ -> true)
                (left @ right @ top @ bot)

    (**
     * [in_board pos] is true iff [pos] is within the dimensions of a 10x10
     * board. In other words, -1 < both values in pos < 10. False otherwise
     *)
    let in_board pos =
        if (fst pos) < 0 || (fst pos) > 9 then false
        else
            if (snd pos) < 0 || (fst pos) > 9 then false
            else true

    (**
     * [check_pos_one board b pos] is a tuple t. Fst t is true iff a piece at
     * [pos] has field player that is equal to b. False otherwise. Snd t is
     * a string containing the reason why the move is not valid. "" if valid.
     *)
    let check_pos_one board b pos =
         match (search pos board) with
            | None -> (false, "There's nothing at (" ^ (string_of_int (fst pos))
                                ^ ", " ^ (string_of_int (snd pos)) ^ ")!")
            | Some x ->
                if x.player = b then (true, "")
                else (false, "That's not your piece!")

    (* See board.mli *)
    let is_valid_move (board:t) (b:bool) (pos_one:position) (pos_two:position)
            : bool * string =
        let within_board = (in_board pos_one) && (in_board pos_two) in
        if (within_board = false) then (false, "Position outside of board") else
        let valid_pos_one = check_pos_one board b pos_one in
        if (fst valid_pos_one) = false then
            valid_pos_one
        else
            let pos_one_p = match (search pos_one board) with
                        | None -> failwith "Nothing at pos_one for some reason"
                        | Some x -> x in
            let possible_moves_list =
                                get_possible_moves board b pos_one_p pos_one in
            if (List.mem pos_two possible_moves_list) = false then
                (false, "That piece can't move there!")
            else
                match (search pos_two board) with
                | None -> (true, "")
                | Some x ->
                    if (x.player = b) then (false,"Don't attack your own team!")
                    else
                        (true, "")

    (**
     * [remove_optional piece] is p in [piece], which is (Some p).
     * Raises:
     *  - Failure if [piece] is None
     *)
    let remove_optional piece =
        match piece with
        | None -> failwith "None"
        | Some p -> p

    (**
     * [execute_conflict board p_two pos_one pos_two] is a tuple containing
     * the new board after executing the movement of the piece from [pos_one]
     * to [pos_two] on [board] and the list of pieces captured in the conflict.
     *)
    let execute_conflict board p_two pos_one pos_two =
        let p_one = remove_optional (search pos_one board) in
        match (p_one.rank, p_two.rank) with
        | (1, 10) ->
            let temp_board = add_mapping pos_one None board in
            let seen_piece = {p_one with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_piece) temp_board in
            (new_board, [p_two])
        | (3, 0) ->
            let temp_board = add_mapping pos_one None board in
            let seen_piece = {p_one with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_piece) temp_board in
            (new_board, [p_two])
        | (_, 0) ->
            let temp_board = add_mapping pos_one None board in
            let p = remove_optional (search pos_two temp_board) in
            let seen_p = {p with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_p) temp_board in
            (new_board, [p_one])
        | (_, 11) ->
            let temp_board = add_mapping pos_one None board in
            let seen_piece = {p_one with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_piece) temp_board in
            (new_board, [p_two])
        | (r1, r2) when r1 < r2 ->
            let temp_board = add_mapping pos_one None board in
            let p = remove_optional (search pos_two temp_board) in
            let seen_p = {p with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_p) temp_board in
            (new_board, [p_one])
        | (r1, r2) when r1 = r2 ->
            let temp_board = add_mapping pos_one None board in
            let new_board = add_mapping pos_two None temp_board in
            (new_board, [p_one; p_two])
        | (r1, r2) when r1 > r2 ->
            let temp_board = add_mapping pos_one None board in
            let seen_piece = {p_one with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_piece) temp_board in
            (new_board, [p_two])
        | _ -> failwith "Invalid tuple passed"

    (**
     * [string_from_tuple t] is the string form of the tuple [t]. i.e. "(x, y)"
     *)
    let string_from_tuple t =
        let (x, y) = t in
        "("^(string_of_int x)^", "^(string_of_int y)^")"

    (**
     * [get_msg pos_one pos_two tup] is the message to be posted to the user
     * depending on how (or if) a conflict occured.
     *)
    let get_msg pos_one pos_two tup =
        match (snd tup) with
        | [] -> ""
        | h::[] ->
            if h.player then
                let p_win = remove_optional (search pos_two (fst tup)) in
                "AI's "^(string_from_piece p_win)^" ("
                ^(string_of_int (get_rank p_win))^")"
                ^" defeated the User's "^(string_from_piece h)^" ("^
                (string_of_int (get_rank h))^")"^"! \nAI's piece is at "
                ^(string_from_tuple pos_two)^" and User's piece has been taken"
                ^" from the board."
            else
                let p_win = remove_optional (search pos_two (fst tup)) in
                "User's "^(string_from_piece p_win)^" ("
                ^(string_of_int (get_rank p_win))^")"
                ^" defeated the AI's "^(string_from_piece h)^" ("
                ^(string_of_int (get_rank h))^
                ")"^"!\nUser's piece is at "
                ^(string_from_tuple pos_two)^" and AI's piece has been taken"
                ^" from the board."

        | h1::h2::[] ->
            "Both the user and the AI's "^(string_from_piece h1)^" have been"
            ^" taken from the board."
        | _ -> failwith "Invalid captured pieces list given"

    (**
     * [check_if_win lst] is true iff [lst] contains a flag (rank 11) piece
     *)
    let check_if_winner lst =
        match lst with
        | [] -> false
        | h1::[] -> if h1.rank = 11 then true else false
        | _ -> false

    (* See board.mli file *)
    let make_move (board:t) (pos_one:position) (pos_two:position)
            : (victory * piece list * string) =
        let (new_board, captured) =
            (match (search pos_two board) with
            | None ->
                let temp_board =
                            add_mapping pos_two (search pos_one board) board in
                ((add_mapping pos_one None temp_board), [])
            | Some piece -> execute_conflict board piece pos_one pos_two) in
        let msg = get_msg pos_one pos_two (new_board, captured) in
        let p_one = remove_optional (search pos_one board) in
        let move_msg =
            if p_one.player then
                ("Player moved "^(string_from_piece p_one)^" from "
                                ^(string_from_tuple pos_one)
                                ^" to "^(string_from_tuple pos_two))
            else
                ("AI moved piece from "^(string_from_tuple pos_one)
                                ^" to "^(string_from_tuple pos_two)) in

        if (check_if_winner captured) then
            let winner = (List.hd captured).player in
            let congrats =
                (if winner then
                    "The AI won the game! Better luck next time!"
                else
                    "Congrats! You won the game!") in
            (Victory (not(List.hd captured).player), captured, congrats)
        else
            (Active (new_board), captured, move_msg^"\n"^msg)

    (* See board.mli file *)
    let rec fill board filled remaining pos player =
        let next_pos = function
        |(9,y) -> if player then (0, y+1) else (0, y-1)
        |(x,y) -> (x+1, y) in
        match remaining with
        | [] -> board
        | h::t ->
            if List.mem pos filled then
                fill board filled remaining (next_pos pos) player
            else
                let new_board = add_mapping pos (Some h) board in
                fill new_board (pos::filled) t (next_pos pos) player

    (* See board.mli file*)
    let do_setup board player =
        let () = Random.self_init () in
        let flag_x_pos = Random.int 10 in
        let flag_pos = (flag_x_pos, (if player then 0 else 9)) in
        let bomb_one_x = match flag_x_pos with
                        |0 -> (Random.int 8) + 2
                        |n -> n-1 in
        let bomb_two_x = match flag_x_pos with
                        |9 -> (Random.int 8)
                        |n -> n+1 in
        let n = if player then 0 else 9 in
        let flag_pos = make_position (fst flag_pos) (snd flag_pos) in
        let bomb_one_pos = make_position bomb_one_x n in
        let bomb_two_pos = make_position bomb_two_x n in
        let three_pos = if player then n+1 else n-1 in
        let bomb_three_pos = make_position flag_x_pos three_pos in
        let flag_board = add_mapping flag_pos
                (Some(make_piece 11 player false)) board in
        let one_bomb_board = add_mapping bomb_one_pos
                (Some (make_piece 0 player false)) flag_board in
        let two_bomb_board = add_mapping bomb_two_pos
                (Some (make_piece 0 player false)) one_bomb_board in
        let three_bombs_board = add_mapping bomb_three_pos
                (Some (make_piece 0 player false)) two_bomb_board in
        let filled = [flag_pos; bomb_one_pos; bomb_two_pos; bomb_three_pos] in
        let remaining = match get_list_all_pieces player with
                        (*The following line removes the flag and three bombs
                         *from the list of pieces that need to be placed because
                         *they have already been put on the board*)
                        |f::b1::b2::b3::t -> t
                        |_ -> failwith "the function should match the above" in
        let shuffled = List.sort (fun x y -> Random.int 2) remaining in
        let n2 = if player then 0 else 9 in
        fill three_bombs_board filled shuffled (make_position 0 n2) player

end