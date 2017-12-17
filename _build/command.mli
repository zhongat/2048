(* [command] is a type that represents the user inputs of the game. *)
type command =
  | Up1
  | Down1
  | Left1
  | Right1
  | Invalid

(* [parse_command command] parses the given key press into a char. It
 * returns a command depending on the given input character. *)
val parse_command : char -> command
