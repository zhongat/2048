open Graphics
open Board
open State
open Command

(* [get_best_move st d] takes in the current state [st], determines the next [d]
 * moves that could be played in each of the four possible directions a tile
 * can move, and returns the best move to next take. *)
val get_best_move : State.state -> int -> Command.command
