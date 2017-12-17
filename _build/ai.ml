open Graphics
open Board
open State
open Command

(* [agent] is a type that represents the two nodes in the AI search space:
 *  - [PLAYER]: the max node in which the player chooses a move out of the four
      possible directions a tile can move.
 *  - [BOARD]: the chance node in which the board inserts a random tile. *)
type agent =
  | PLAYER
  | BOARD

(* [expectimax_helper s d a] maximizes the score by determining the which move
 * will result in a state that returns the highest score out of the four
 * possible directions a tile can be moved. [expectimax_helper] is given a
 * state [s], a depth [d], and an agent [a]. *)
let rec expectimax_helper s d a =
  if d = 0 then get_score s
  else if a = BOARD then
    let score = min_int in
    let stateup = update_state Up1 s in
    let statedown = update_state Down1 s in
    let stateleft = update_state Left1 s in
    let stateright = update_state Right1 s in
    let new_states = [stateup; statedown; stateleft; stateright] in
    let rec expectimax_board_helper states score =
      match states with
      | [] -> score
      | h::t ->
        let new_score_four = expectimax_helper h (d - 1) PLAYER in
        let compute_four_score score new_score =
          if new_score = min_int then score else score + (new_score / 10) in
        let new_score_two = expectimax_helper h (d - 1) PLAYER in
        let compute_two_score score new_score =
          if new_score = min_int then score else score + (new_score / 10) * 9 in
        let score' = compute_four_score score new_score_four in
        let score'' = compute_two_score score' new_score_two in
        expectimax_board_helper t score'' in
    expectimax_board_helper new_states score
  else
    let score = min_int in
    let stateup = update_state Up1 s in
    let statedown = update_state Down1 s in
    let stateleft = update_state Left1 s in
    let stateright = update_state Right1 s in
    let new_states = [stateup; statedown; stateleft; stateright] in
    let rec expectimax_player_helper states score =
      match states with
      | [] -> score
      | h::t ->
        let new_score = expectimax_helper h (d - 1) BOARD in
        if new_score > score then new_score else score in
    expectimax_player_helper new_states score

(* [expectimax s d a] returns the highest AI score out of the four possible
 * directions a tile can move given the current state [s], the depth or number
 * of steps to take [d], and an agent [a]. *)
let expectimax s d a =
  let score = expectimax_helper s d a in
  score / 4

let get_best_move s d =
  let score = min_int in
  let moves = [Up1; Down1; Left1; Right1] in
  let rec move_helper moves best_move score s d =
    match moves with
    | [] -> best_move
    | h::t -> begin
      match h with
      | Up1 ->
        let s' = update_state Up1 s in
        let new_score = expectimax s' (d - 1) BOARD in
        if new_score > score then move_helper t Up1 new_score s d
        else move_helper t best_move score s d
      | Down1 ->
        let s' = update_state Down1 s in
        let new_score = expectimax s' (d - 1) BOARD in
        if new_score > score then move_helper t Down1 new_score s d
        else move_helper t best_move score s d
      | Left1 ->
        let s' = update_state Left1 s in
        let new_score = expectimax s' (d - 1) BOARD in
        if new_score > score then move_helper t Left1 new_score s d
        else move_helper t best_move score s d
      | Right1 ->
        let s' = update_state Right1 s in
        let new_score = expectimax s' (d - 1) BOARD in
        if new_score > score then move_helper t Right1 new_score s d
        else move_helper t best_move score s d
      | _ -> failwith "Not a valid move"
    end in
  move_helper moves Up1 score s d
