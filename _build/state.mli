open Command

(* [state] is a type that represents the current state of a 2048 game. [state]
 * contains information on the current score, the arrangement of tiles or values
 * on the board, and the status of the game. The score indicates the score
 * of the board based on the tile with the largest value that has been created
 * by merging other tiles. The board stores the values and positions of the
 * tiles that are currently on the board. The status is represented by the
 * value -1, 0, or 1, where -1 indicates that the user has lost, 0 indicates
 * that the game in progress, and 1 indicates that the user has won. *)
type state = {score: int; board: int list; status: int}

(* [get_status st] is the current status of a game whose current state is
 * represented by [st]. *)
val get_status : state -> int

(* [get_score st] is the current score of a game whose current state is
 * represented by [st]. *)
val get_score : state -> int

(* [get_board st] is the current board of the game whose current state is
 * represented by [st]. *)
val get_board : state -> int list

(* [init_state] returns an initial board state with 2 blocks in random
 * positions on the board, each with a value of either 2 or 4. *)
val init_state : state

(* [update_state c] is the updated/resulting state based on the command [c].
 * The resulting state moves and merges the blocks based on the rules of 2048:
 * - all blocks on the board shift in the direction of the arrow key specified
 *   by [c]
 * - for each pair of adjacent blocks with the same value, the two blocks
 *   merge together into one block with a value equal to the sum of the
 *   individual values of the original two blocks
 * - a new tile with a value of either 2 or 4 is randomly generated and added
 *   to the board. *)
val update_state : Command.command -> state -> state
