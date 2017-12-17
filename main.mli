open Graphics
open Board
open State
open Command

(* [play_game st1 st2] takes in two initial states and executes the game. Once
 * [play_game] is executed, it will constantly listen for user key inputs
 * (keys W, A, S, D, and arrow keys left, right, up, and down) and plays the
 * next state for each player. If inputs other than the keys W, A, S, D; the
 * arrow keys; and Esc are pressed, then the state will not change. The game
 * will stop when a player reaches the winning score, loses, or quits (presses
 * Esc). *)
val play_game : State.state -> State.state -> unit
