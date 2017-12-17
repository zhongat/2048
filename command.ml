type command =
  | Up1
  | Down1
  | Left1
  | Right1
  | Invalid

let parse_command c =
  match c with 
 | 'w' -> Up1
 | 'a' -> Left1
 | 's' -> Down1
 | 'd' -> Right1
 | 'i' -> Up1
 | 'k' -> Down1
 | 'l' -> Right1
 | 'j' -> Left1
 | _ -> Invalid
