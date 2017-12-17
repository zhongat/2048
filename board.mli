open Graphics
open State

(* [draw_board st] takes in a state, the x position of the grid, and the y
 * position of the grid and draws a 4x4 board with the values
 * provided by [st]. Drawing the board may involve generating new tiles,
 * changing the position of tiles, and changing the values of current tiles
 * when tiles are merged and moved around on the board. *)
val draw_board: int -> int -> State.state -> unit
