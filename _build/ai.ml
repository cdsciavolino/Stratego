open Board.GameBoard

module type AI = sig
  type board = t
  type victory = Board.GameBoard.victory
  type piece = Board.GameBoard.piece
  val choose_best_board : board -> (victory * piece list * string)
end

module GameAI : AI = struct

  (* See board.mli for detailed information about board, piece, and victory *)
  type board = t
  type victory = Board.GameBoard.victory
  type piece = Board.GameBoard.piece

  (* [replace_pos board lst] is a new board with replacements made according
   * to the (pos,piece option) association list [lst], where the first of every
   * tuple is the pos that needs to be overwritten and the second is the piece
   * that goes in that position
   *)
  let replace_pos board lst =
    let newb = ref (empty_board ()) in
    let f k v =
        newb := (add_mapping k (try List.assoc k lst with _ -> v) !newb) in
    let () = board_iter (fun k v -> f k v) board in
    !newb

  (* [can_defeat init rank] is the number of pieces during the initial setup of
   * the game that a piece of rank [rank] could tie or defeat.
   * For example, at the beginning of the game, there are 10 pieces that a
   * scout (rank 2) can defeat -- the enemies bomb, flag and 8 scouts.
   *
   * Requires: [rank] : int in [0,11]
   *)
  let can_defeat_init = function
  | 0 -> 35  | 1 -> 2  | 2 -> 10  | 3 -> 21  | 4 -> 19  | 5 -> 23  |6 -> 27
  | 7 -> 30  | 8 -> 32 | 9 -> 33  | 10  -> 33  |11 -> 0
  | n -> failwith "rank should be in [0,11]"

  (* [get_probability rank player] is the probability that an AI piece of [rank]
   * can defeat a randomly selected piece belonging to the player that is
   * still on the board.
   *
   * Requires: [rank] : int
   *)
  let get_probability rank =
    let captured_array = user_pieces_lost in
    let captured_list = captured_array |> Array.to_list in
    let filtered = List.filter (fun p -> get_rank p <> 12) captured_list in
    let f = match rank with
            | 3 -> (fun p -> get_rank p <= 3)
            | 1 -> (fun p -> get_rank p = 10)
            | n -> (fun p -> get_rank p <= rank && get_rank p <> 0) in
    let can_beat_captured = List.filter f filtered in
    let can_beat_uncap = (can_defeat_init rank) -
                                            (List.length can_beat_captured) in
    let prob = (float can_beat_uncap) /. float(40 - List.length filtered) in
    prob

  (* [ai_battle p1 p2] is a piece option representing the piece that ai assumes
   * will win if the AI's piece, p1, battles the player's piece, p2. AI assumes
   * p1 will win under the following circumstances:
   *
   *  1) p1 is a miner and p2 is a bomb
   *  2) p1 is any piece and p2 is the player's flag
   *  3) p1 is a spy (rank 1) and p2 is the marshal (rank 10)
   *  4) The AI has not battled p2 in the past and the rank(p1) <= rank(p2) + 2
   *  5) The AI has battle p2 in the past and rank(p1) > rank(p2)
   *
   * In all other circumstances, the AI assumes p1 will lose to p2 in a battle.
   *
   * Requires:
   * [p1] and [p2] : piece
   * [p2] is the player's piece
   *)
  let ai_battle p1 p2 =
    match (get_rank p1), (get_rank p2) with
    | _, _ when (not (get_player p2)) -> failwith "ai shouldn't battle itself"
    | 3,0 when get_been_seen p2-> Some p1
    | _ , 0 when get_been_seen p2-> Some p2
    | _ , 11 when get_been_seen p2 -> Some p1
    | 1, 10 when get_been_seen p2 -> Some p1
    | p1r, p2r when (get_been_seen p2) ->
        if p1r > p2r then Some p1 else if p1r < p2r then Some p2 else None
    | p1r, p2r ->
        let () = Random.self_init () in
        let n = Random.int 101 |> float in
        if (100. *. get_probability p1r) > n then Some p1 else Some p2


  (* [ai_move board pos1 pos2] is the board that the AI assumes will result when
   * the piece at position [pos1] is moved to position [pos2] on board [board].
   * If moving that piece causes a battle (i.e. there is an enemy piece at pos2)
   * then the board returned reflects what the AI thinks the outcome of the
   * battle will be according to [ai_battle].
   *
   * Requires:
   * [board] : Board
   * [pos1], [pos2] : Board.position
   * [pos1] <> [pos2]
   * There is a piece at [pos1] on [board]
   *)
  let ai_move board pos1 pos2 =
    if pos1 = pos2 then failwith "can't move to same position" else
    match search pos1 board, search pos2 board with
    | None, _ -> failwith "there's no piece here to move"
    | p1 , None -> replace_pos board [(pos1, None);(pos2, p1)]
    | Some p1,Some p2 ->replace_pos board [(pos1, None);(pos2, ai_battle p1 p2)]


  (* [get_value rank] returns the AI's perceived value of a given piece's rank.
   * The values of pieces are how the AI determines which move it should make.
   *
   * For most pieces, the AI's perceived value of a rank is simply that rank.
   * However, there are some exceptions, such as spies. The reason for this is
   * as follows: despite the fact that a spy is rank 1 and it is defeated in
   * battle by nearly every piece, it is valuable because without it, it would
   * be very difficult to defeat the most powerful piece in the game, the
   * marshal. So losing a spy is very costly and so it is assigned a higher
   * value than 1. Similar arguments apply for the flag (obviously the most
   * important piece because its capture ends the game), bombs, and miners.
   *
   * Note that this function takes in an int, which is the integer
   * representation of rank described in the [get_rank] function of the Board
   * module.
   *
   * Requires:
   * [rank] : int
   *)
  let get_value = function
    |0 -> 5
    |1 -> 6
    |3 -> 5
    |11 -> 10000
    |n -> n

  (* [score board] returns the AI's net score on [board] by going through
   * the AI's pieces, summing their values, doing the same for the player's
   * pieces,and subtracting player's score from the AI's score to find the net
   * score.
   *
   * The scoring heuristic assigns each piece an integer value based on its
   * rank. The value of each rank is as described in [get_value]
   *
   * Requires: [board] : board
   *)
  let score board =
    let f piece a = (match piece with
        |None -> a
        |Some p when (get_player p) -> a-(get_value (get_rank p))
        |Some p -> a+(get_value (get_rank p))) in
    board_fold (fun k v ac -> f v ac) board 0

  (* [can_move_to board (x,y) player)] returns true if a piece belonging to
   * [player] (i.e. true if the user, false if the AI) can move to coordinate
   * [(x,y)] on [board]; that is, either there is an enemy piece at [(x,y)] or
   * no piece is at [(x,y)].
   *
   * Requires:
   * [(x,y)] : (int*int)
   * [board] : Board
   * player : bool
   *)
  let can_move_to board (x,y) player =
    try
      match search (make_position x y) board with
      |None -> true
      |Some piece -> player <> (get_player piece)
    with
    |_ -> false

  (* [has_move piece] returns true iff there is 1 or more valid move that the
   * piece at position [pos] can make on [board] when it is the turn of [player]
   * i.e. [player] = true means it is the player's turn; [player] = false means
   * it is the AI's turn.
   *
   * Requires:
   * [board] : Board
   * [pos] : (int*int)
   * [player] : bool
   * [search pos board] = Some [Board.piece]
   *
   *)
  let has_move board pos player=
    let piece = match search pos board with
               |Some p -> p
               | _ -> failwith "Should be a piece here" in
    if ((get_rank piece) = 0 || (get_rank piece) = 11) then
      false
    else
      let (x,y) = get_tuple pos in
        let can_up = (match (x,y+1) with
                   |(x',y') when y' > 10 -> false
                   |(x',y') -> can_move_to board (x',y') player) in
        let can_down = (match (x,y-1) with
                     |(x',y') when y' < 0 -> false
                     |(x',y') -> can_move_to board (x',y') player) in
        let can_left = (match (x-1,y) with
                     |(x',y') when x < 0 -> false
                     |(x',y') -> can_move_to board (x',y') player) in
        let can_right = (match (x+1,y) with
                      |(x',y') when x > 10 -> false
                      |(x',y') -> can_move_to board (x',y') player) in
        (can_up || can_down || can_left || can_right)


  (* [get_moveable board] returns the list of positions in [board] that
   * contain a piece that can make 1 or more valid moves. Note that positions
   * in this list are represented as (int*int).
   *
   * Requires:
   * [board] : Board
   * [player] : bool
   *)
  let get_moveable board player =
    let lst = ref [] in
    let f k = function
      | Some p when (get_player p) = player -> has_move board k player
      | _ -> false in
    let () = board_iter
      (fun k v -> if f k v then (lst := k::(!lst)) else ()) board in
    !lst

  (* [get_moves_piece board pos] is an (pos1,pos2) association list that
   * represents all of the posistions the piece at [pos] can move to. The
   * starting move is the first in the association list.
   *
   * Requires:
   * [search board pos] <> None
   * [board] : Board
   * [pos] : Board.position
   *)
  let get_moves_piece board pos  =
    let moves = (match search pos board with
    | None -> [] (*You shouldn't be calling this on an empty position*)
    | Some p -> get_possible_moves board (get_player p) p pos) in
    List.fold_left (fun a x -> (pos, x)::a) [] moves

  (* [make_winning_board player] is a board with only two pieces on it,
   *    [player]'s flag and a moveable piece of [player].  This board is used
   *    so the ai can score a victory as very high (or low, depending on
   *    [player]).  If there were only a flag, ai would perceive the board as
   *    a draw.
  *)
  let make_winning_board player =
        let flag = make_piece 11 player false in
        let moveable = make_piece 3 player false in
        let pos0 = make_position 0 0 in
        let pos1 = make_position 0 1 in
        empty_board () |> add_mapping pos0 (Some flag)
          |> add_mapping pos1 (Some moveable)


  (**
   * [get_valid_boards board player] is a (board, move) association list that
   * represents all of the moves [player] can take given board [board] and the
   * board after that move has been made.  In the case that a move creates a
   * board where the user wins, a board is returned with two pieces on it:
   * the user's flag and a moveable user piece.
   *  Requires:
   *      board : board
   *      player: bool (true when user, false when ai)
   *)
  let get_valid_boards board player =
    let moveable = get_moveable board player in
    let moves = List.fold_left
        (fun a x -> ((get_moves_piece board x) @ a)) [] moveable in
    if (not player) then List.fold_left
        (fun a (p1,p2) -> (ai_move board p1 p2, (p1,p2))::a) [] moves
    else
        let new_board pos1 pos2 = match make_move board pos1 pos2 with
                        | (Active brd, _, _) -> brd
                        | (Victory b, _ ,_) -> make_winning_board b in 
        List.fold_left
        (fun a (p1,p2) -> (new_board p1 p2,(p1,p2))::a) [] moves


  (* [break_tie move1 move2 player] is a move chosen assuming a forward move is
   * better than a sideways move which is better than a backwards move.
   * More specifically, we chose move by looking at [move1] and [move2]:
   *      - if both move [player] backwards, a random move is chosen
   *      - if both move [player] forward, a random move is chosen
   *      - if only one moves [player] backward, the other move is chosen
   *      - if only one moves [player] sideways, the other move is chosen
   *
   * Requires:
   * [move1], [move2] : (position*position)
   * [player] : bool, true when the player is making the move
   *
   *)
  let break_tie move1 move2 player =
    let (from1_x,from1_y) = get_tuple (fst move1) in
    let (to1_x, to1_y) = get_tuple (snd move1) in
    let (from2_x, from2_y) = get_tuple (fst move2) in
    let (to2_x, to2_y) = get_tuple (snd move2) in
    let backward2 = if player then to2_y < from2_y else to2_y > from2_y in
    let backward1 = if player then to1_y < from1_y else to1_y > from1_y in
    let sideway2 = from2_x <> to2_x in
    let sideway1 = from1_x <> to1_x in
    let random = if Random.bool () then move1 else move2 in
    match backward1, backward2 with
    | true, true -> random
    | true, false -> move2
    | false, true -> move1
    | _ -> if sideway1 = sideway2 then random
           else if sideway1 then move2
           else move1

  (* [minimax board min depth] is the resulting (score, move) from the
   * minimax algorithm, which looks at future moves until depth [depth].  When
   * [min] the move that produces the smallest score is chosen, when not [min]
   * the move that produces the largest score is chosen.
   * Score is:
   *    - (score board) when there is a valid move resulting in normal game play
   *    - (2000) when the user has no moves left but the ai does
   *    - (-2000) when the ai has no moves left but the user does
   *    - (0) when both players are out of moves or [depth] = 0
   * Move is a (pos1, pos2) tuple where:
   *    - pos1, pos2 are valid board positions when [player] has a move
   *    - pos2,pos2 = (-1,-1) when one or more players is out of moves or
   *        [depth] = 0
   * Requires:
   *    min : bool,true when you want the minimum score (player = user)
   *    board: board
   *    depth : int
   *)
  let rec minimax board min depth =
      let invalid = make_position (-1) (-1) in
      let no_move = (invalid, invalid) in
      let worst_min = (2000, no_move) in
      let worst_max = (-2000, no_move) in
      let tie = (0, no_move) in
      if depth = 0 then (score board, no_move) else
      match get_valid_boards board min, min with
      | [], true  -> if get_valid_boards board false =[] then tie else worst_min
      | [], false -> if get_valid_boards board true = [] then tie else worst_max
      | lst, true -> List.fold_left (fun a x -> get_min a x depth) worst_min lst
      | lst,false -> List.fold_left (fun a x -> get_max a x depth) worst_max lst


  (* [get_max (s1, m1) (b2, m2) depth] chooses the tuple that represents the
   * (score, move) that maximizes the board score.  In the case of a tie, the
   * result is random.
   *      Score: [s1] or the score from (minimax [b2] true  [depth - 1])
   *      Move: [m1] or [m2] where [m1] produces score [s1]
   *            when minimaxed at depth [depth - 1] and [m2] produces board [b2]
   *            when [m2] is played on some board that represents a game state
   *      Requires:
   *            (s1,m1) : int*(position*position)
   *            (b2,m2) : board*(position*position)
   *)
  and get_max (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 true (depth - 1) in
      if s1 > s2 then
        (s1, m1)
      else if (s1=s2 && (break_tie m1 m2 false) = m1) then (s1,m1)
      else
        (s2, m2)


  (* [get_min (s1, m1) (b2, m2) depth] chooses the tuple that represents the
   * (score, move) that minimizes the board score.  In the case of a tie, the
   * result is random.
   *      Score: [s1] or the score from (minimax [b2] false [depth - 1])
   *      Move: [m1] or [m2] where [m1] produces score [s1]
   *            when minimaxed at depth [depth - 1] and [m2] produces board [b2]
   *            when [m2] is played on some board that represents a game state
   *      Requires:
   *            (s1,m1) : int*(position*position)
   *            (b2,m2) : board*(position*position)
   *)
  and get_min (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 false (depth-1) in
      if s1 < s2 then
        (s1, m1)
      else if (s1=s2 && (break_tie m1 m2 true) = m1) then (s1,m1)
      else (s2, m2)

  (* [choose_best_board board] is a victory variant that represents the game
   * after ai has made a move. The vitory variant is:
   *      - Victory(true) if either player has run out of moves
   *      - Victory(false) if the ai has captured the user's flag
   *      - Active(board) if the game is ongoing
   * and is calculated based on the move ai decides is most optimal for the ai,
   * according to the minimax algorithm.
   * Requires: [board] : board
   *)
  let choose_best_board board =
    let move = snd (minimax board false 3) in
    let pos1 = fst move in
    let pos2 = snd move in
    if pos1 = (make_position (-1) (-1)) then
        (Victory true, [], "")
    else
        make_move board pos1 pos2
end

