open Command

type tile = int

type state = {score: int; board: int list; status: int}

(* [get_status s] gets the current status of the game where -1
 * indicates that the user has lost, 0 indicates that the game in progress,
 * and 1 indicates that the user has won *)
let get_status s = s.status

(* [get_score s] gets the score the player. The score is the addition of
 * all the tiles currently displayed on the board *)
let get_score s = s.score

(* [get_board s] gets the board of the current game. The board contains the
 * values of the 16 tiles on the board. *)
let get_board s = s.board

(* [convert_begin board] converts the format in the [board] so that the board
 * can be represented as a list of each row in the board, where the row is
 * a list. *)
let convert_begin board =
  let rec helper l acc =
    match l with
    | [] -> acc
    | a::b::c::d::t -> helper t ([a;b;c;d]::acc)
    | _ -> failwith "wrong length" in
  List.rev (helper board [])

(* [convert_end board] converts the format of the [board] where the current
 * board is in the format of the list of rows into simply a list of the values
 * of all the tiles. *)
let convert_end board =
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t -> helper t (acc @ h) in
  helper board []

(* [merger values in_order score] returns a tuple of the merged values and
 * score. It merges a single row of [values] so that if the
 * values are the same then a new tile of the two added together is generated.
 * The values should not contain a zero tile. If [in_order] is true the values
 * will merge in order otherwise they will merge in the reverse order. The
 * [score] updates when the tiles are merged such that the value of the newly
 * created tile is the added onto the score. *)
let merger values in_order score=
  let rec helper l acc s =
    match l with
    | [] -> acc, s
    | one::two::t ->
      if one = two then helper t (acc @ [one+two]) (s+one+two)
      else helper (two::t) (acc @ [one]) s
    | one::[] -> (acc @ [one]), s in
  if in_order then
    helper values [] score else
      let temp = (helper (List.rev values) [] score) in
      (List.rev (fst temp)), (snd temp)

(* [remove_zero row] removes the zero tiles from [row]. *)
let remove_zero row =
  let rec helper l acc =
    match l with
    | [] -> acc
    | 0::t -> helper t acc
    | h::t -> helper t (h::acc) in
  List.rev (helper row [])

(* [fill_zero_up board front] fills the empty tiles with zeroes so that each
 * row in the board will have 4 values. If [front] is true then the zeroes
 * will be added to the front of the row in [board]. Otherwise, they are
 * added to the end of the row. *)
let fill_zero_up board front=
  let rec add_zero num acc =
    begin
      match num with
      | 0 -> acc
      | x -> add_zero (x-1) (acc @ [0])
    end in
  let rec rev_add_zero num acc =
    begin
      match num with
      | 0 -> acc
      | x -> rev_add_zero (x-1) (0::acc)
    end in
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t ->
      let missing = (4 - (List.length h)) in
      if front then helper t ((rev_add_zero missing h)::acc)
      else helper t ((add_zero missing h)::acc) in
  List.rev (helper board [])

(* [col_to_row l] will change the columns in [l] into rows so that the nested
 * lists in [l] will have each list's same index reformatted into a
 * new list together so that the original representation [l] as a list of
 * columns becomes a list of rows. *)
let col_to_row l =
  let col0 = List.hd l in
  let col1 = List.nth l 1 in
  let col2 = List.nth l 2 in
  let col3 = List.nth l 3 in
  let rec helper c0 c1 c2 c3 acc =
    match (c0,c1,c2,c3) with
    | h0::t0, h1::t1, h2::t2, h3::t3 -> helper t0 t1 t2 t3 ([h0;h1;h2;h3]::acc)
    | _ -> acc in
  List.rev (helper col0 col1 col2 col3 [])

(* [up_move board score] takes in the current [board] and performs the up
 * operation on it where all the tiles moves upward and merge when met with a
 * tile of the same value. It also takes in the current [score] and updates it
 * to reflect the new board. The function returns a tuple of the
 * new board and new score. *)
let up_move board score =
  let l = convert_begin board in
  let row0 = List.hd l in
  let row1 = List.nth l 1 in
  let row2 = List.nth l 2 in
  let row3 = List.nth l 3 in
  let rec helper r0 r1 r2 r3 acc s =
    match (r0,r1,r2,r3) with
    | h0::t0, h1::t1, h2::t2, h3::t3 ->
      let temp = [h0;h1;h2;h3] |> remove_zero in
      let temp', score' = merger temp true s in
      helper t0 t1 t2 t3 (temp'::acc) score'
    | _ -> acc, s in
  let new_board, new_score = (helper row0 row1 row2 row3 [] score) in
  let temp_board = List.rev new_board  in
  let final_board =
    (fill_zero_up temp_board false) |> col_to_row |> convert_end in
  final_board, new_score

(* [down_move board score] returns an updated board and updated score.
 * It takes in the current [board] and performs the down
 * operation on it where all the tiles moves downward and merge when met with a
 * tile of the same value. The [score] also updates when the [board] is able
 * to merge. *)
let down_move board score=
  let l = convert_begin board in
  let row0 = List.hd l in
  let row1 = List.nth l 1 in
  let row2 = List.nth l 2 in
  let row3 = List.nth l 3 in
  let rec helper r0 r1 r2 r3 acc s =
    match (r0,r1,r2,r3) with
    | h0::t0, h1::t1, h2::t2, h3::t3 ->
      let temp = [h0;h1;h2;h3] |> remove_zero in
      let temp', score' = merger temp false s in
      helper t0 t1 t2 t3 (temp'::acc) (score')
    | _ -> acc, s in
  let new_board, new_score = (helper row0 row1 row2 row3 [] score) in
  let temp_board = List.rev new_board in
  let final_board =
    (fill_zero_up temp_board true) |> col_to_row |> convert_end in
  final_board, new_score

(* [left_move board] returns an updated board and updated score.
 * It takes in the current [board] and performs the left
 * operation on it where all the tiles moves leftward and merge when met with a
 * tile of the same value. The [score] also updates when the [board] is
 * able to merge.*)
let left_move board score =
  let l = convert_begin board in
  let rec helper rows acc s=
    match rows with
    | [] -> acc, s
    | h::t ->
      let temp = remove_zero h in
      let temp', score' = merger temp true s in
      helper t (temp'::acc) score' in
  let new_board, new_score = (helper l [] score) in
  let temp_board = List.rev new_board in
  let final_board = fill_zero_up temp_board false |> convert_end in
  final_board, new_score

(* [right_move board score] returns an updated board and updated score.
 * It takes in the current [board] and performs the right operation on it
 * where all the tiles moves rightward and merge when met with a
 * tile of the same value. The [score] also updates when the [board] is able to
 * merge. *)
let right_move board score =
  let l = convert_begin board in
  let rec helper rows acc s=
    match rows with
    | [] -> acc, s
    | h::t ->
      let temp = remove_zero h in
      let temp', score' = merger temp false s in
      helper t (temp'::acc) score' in
  let new_board, new_score = (helper l [] score) in
  let temp_board = List.rev new_board in
  let final_board = fill_zero_up temp_board true |> convert_end in
  final_board, new_score

(* [count_empty b] is the number of zeros in [b]. This is used to count the
 * number of empty tiles (represented by 0) in board b.*)
let count_empty b =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h::t -> if (h=0) then helper t (acc+1) else helper t acc in
  helper b 0

(* [update_status b score] updates the status such that the status becomes -1
 * if the player loses and cannot make any more moves in any direction with [b].
 * The status becomes 1 if the player wins by forming the 2048 tile. Otherwise
 * the status remains as 0, indicating that the game is in progress. *)
let update_status b score =
  let up = fst (up_move b score) in
  let down = fst (down_move b score) in
  let right = fst (right_move b score) in
  let left = fst (left_move b score) in
  if (List.mem 2048 b) then 1
  else if (b!=up && (count_empty up)!=0) then 0
  else if (b!=down && (count_empty down)!=0) then 0
  else if (b!=right && (count_empty right)!=0) then 0
  else if (b!=left && (count_empty left)!=0) then 0
  else -1

(* [gerenate_random b] takes in [b] or the board and creates one random tile
 * of either a 2 with a 90% chance or a tile of 4 with a 10% chance. *)
let generate_random b =
  let pos = (Random.int (count_empty b)) in
  let rand = Random.int 9 in
  let newval = if (rand = 0) then 4 else 2 in
  let rec helper s st num if_added =
    match s with
    | [] -> st
    | h::t ->
      if (h==0 && num=0 && if_added=false) then helper t (newval::st) num true
      else if (h==0) then helper t (h::st) (num-1) if_added
      else helper t (h::st) num if_added in
  List.rev (helper b [] pos false)

(* [init_state] generates the starting board of the player.*)
let init_state =
  Random.init (int_of_float (Unix.time ()));
  let rec empty_board num acc =
    if (num=0) then acc else empty_board (num-1) (0::acc) in
  let b = empty_board 16 [] in
  let b1 = generate_random (generate_random b) in
  {score=0; board=b1; status=update_status b1 0}


(* [update_state command st] takes in the current [st] and [command] to perform
 * the [command] or input on the [st] to generate the next state for the game *)
let update_state command st =
  let state_helper b prev s =
    let b_status = update_status b st.score in
    if (b_status != 0 || (count_empty b = 0 && b_status = 0) || b = prev)
    then {score=s; board=b; status=b_status}
    else
      let b1 = generate_random b in
      {score=s; board=b1; status=update_status b1 s} in
  match command with
  | Up1 -> let b,s = up_move st.board st.score in state_helper b st.board s
  | Down1 -> let b,s = down_move st.board st.score in state_helper b st.board s
  | Left1 -> let b,s = left_move st.board st.score in state_helper b st.board s
  | Right1 ->
    let b,s = right_move st.board st.score in state_helper b st.board s
  | _ -> st
