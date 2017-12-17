 open Graphics
 open Board
 open State
 open Command
 open Ai

(* [display_score x y] displays the [score] at position x: [x] and y: [y]
 * requires: [score] is a valid int
 * [x] and [y] are valid int positions within the gui's range*)
 let display_score score x y =
   moveto x y;
   draw_string ("Score: " ^ (string_of_int score))

(* [repl_1player s1] is the REPL for the classic 1-player game 2048
 * It runs the game by taking in a state [s1], waiting for a valid move,
 * changing the board accordingly until the player either loses
 * or gets 2048
 * requires: [s1] is a valid state *)
let rec repl_1player s1 =
  set_color yellow;
  fill_rect 390 440 85 30;
  set_color black;
  draw_rect 390 440 85 30;
  display_score s1.score 400 448;
  moveto 100 420;
  draw_string "MOVE THE TILES TO TRY TO GET THE TILE 2048";
  moveto 90 80;
  draw_string "Controls: 'wasd' for up, left, down, right";
  if s1.status = -1 then
    (moveto 180 120;
    draw_string "YOU LOST :(";
    play_next_move_1p s1)
  else if s1.status = 1 then
    (moveto 175 80;
    draw_string "YOU WON";
    play_next_move_1p s1)
 else play_next_move_1p s1
and play_next_move_1p s1 =
 let e = wait_next_event [Key_pressed] in
   let newstate = update_state (parse_command e.key) s1 in
   clear_graph ();
   draw_board 120 300 newstate;
   repl_1player newstate

(* [winner s1 s2] displays the winning message for the 2-player
 * game depending on which player has the greater score.
 * requires: [s1] is a valid state
 * [s2] is a valid state *)
let winner s1 s2 =
 if s1.score>s2.score
 then (moveto 175 80; draw_string "PLAYER 1 HAS WON")
 else if s2.score>s1.score
 then (moveto 175 80; draw_string "PLAYER 2 HAS WON")
 else (moveto 175 80; draw_string "TIE")

(* [draw_2board s1 s2] draws 2 board given states [s1] [s2]
 * The board for [s1] appears on the left hand side, the board
 * for [s2] appears on the right hand side *)
let draw_2board s1 s2 =
  draw_board 10 300 s1;
  draw_board 250 300 s2

(* [repl_2player s1 s2] is the REPL for the multiplayer game 2048
 * It runs the game by taking in states [s1], [s2], waiting for a valid move,
 * changing the board accordingly until both players cannot move or a
 * player gets 2048.
 * Whoever gets 2048 first wins
 * If both players cannot move, the player with the higher score wins
 * requires: [s1] is a valid state
 * [s2] is a valid state *)
let rec repl_2player s1 s2 =
  set_color yellow;
  fill_rect 390 440 85 30;
  set_color black;
  draw_rect 390 440 85 30;
  moveto 100 420;
  display_score s2.score 400 448;
  set_color yellow;
  fill_rect 20 440 85 30;
  set_color black;
  draw_rect 20 440 85 30;
  moveto 100 420;
  display_score s1.score 30 448;
  moveto 150 460;
  draw_string "RACE TO GET THE HIGHEST SCORE.";
  moveto 50 410;
  draw_string
    "If any player cannot move, the player with the highest score wins.";
  moveto 100 395;
  draw_string "The first player to get 2048 wins.";
  moveto 90 360;
  draw_string "PLAYER 1";
  moveto 330 360;
  draw_string "PLAYER 2";
  if (s1.status = 1 && s2.status = 1) then
    (moveto 175 100;
    draw_string "TIE";
    repl_2player s1 s2)
  else if (s1.status = -1 && s2.status = -1) then
    (moveto 160 100;
    draw_string "Both players cannot move";
    winner s1 s2;
    play_next_move_2p s1 s2)
  else if s1.status = 1 then
    (moveto 10 80;
    draw_string "PLAYER 1 HAS WON";
    play_next_move_2p s1 s2)
  else if s2.status = 1 then
    (moveto 250 80;
    draw_string "PLAYER 2 HAS WON";
    play_next_move_2p s1 s2)
  else if s1.status = -1 then
    (moveto 60 130;
    draw_string "PLAYER 1 CANNOT MOVE";
    play_next_move_2p s1 s2)
  else if s2.status = -1 then
    (moveto 275 130;
    draw_string "PLAYER 2 CANNOT MOVE";
    play_next_move_2p s1 s2)
  else
    play_next_move_2p s1 s2
(* Function called if player 1 moves *)
and player1_move newstate s2 =
  clear_graph ();
  draw_board 250 300 s2;
  draw_board 10 300 newstate;
  repl_2player newstate s2
(* Function called if player 2 moves *)
and player2_move newstate s1 =
  clear_graph ();
  draw_board 10 300 s1;
  draw_board 250 300 newstate;
  repl_2player s1 newstate
(* Executes the next move given an input keypressed *)
and play_next_move_2p s1 s2 =
  let e = wait_next_event [Key_pressed] in
  match e.key with
  | 'w' | 's' | 'a' | 'd' ->
    let newstate = update_state (parse_command e.key) s1 in
    player1_move newstate s2
  | 'i' | 'j' | 'k' | 'l' ->
    let newstate = update_state (parse_command e.key) s2 in
    player2_move newstate s1
  | _ -> repl_2player s1 s2

(* [repl_ai s] is the REPL for the AI. It takes in an initial state
 * [s] and tries to solve 2048 from there
 * requires: [s] is a valid state *)
let rec repl_ai s =
  set_color yellow;
  fill_rect 390 440 85 30;
  set_color black;
  draw_rect 390 440 85 30;
  display_score s.score 400 448;
  let best_move = get_best_move s 7 in
  let newstate = update_state best_move s in
  clear_graph ();
  draw_board 120 300 newstate;
  Unix.sleepf 0.1;
  repl_ai newstate

(* [repl_vs_ai s1 s2] is the REPL for the player vs. ai game 2048
 * It runs the game by taking in states [s1], [s2], waiting for a valid move,
 * changing the board accordingly until one of the boards cannot move
 * or one of the boards gets 2048.
 * The first board that cannot move loses (despite if the score is higher)
 * requires: [s1] is a valid state
 * [s2] is a valid state *)
let rec repl_vs_ai s1 s2 step=
  moveto 50 440;
  set_color yellow;
  fill_rect 390 440 85 30;
  set_color black;
  draw_rect 390 440 85 30;
  moveto 100 420;
  display_score s2.score 400 448;
  set_color yellow;
  fill_rect 20 440 85 30;
  set_color black;
  draw_rect 20 440 85 30;
  moveto 100 420;
  display_score s1.score 30 448;
  moveto 150 460;
  moveto 50 400;
  draw_string "Try to get a higher score than the AI in less amount of moves!";
  moveto 40 380;
  draw_string
    "If you or the AI cannot move, the other board automatically wins";
  moveto 100 360;
  draw_string "YOU";
  moveto 350 360;
  draw_string "AI";
  if (s1.status = 1 && s2.status = 1) then
    (moveto 175 100;
     draw_string "TIE";
     repl_vs_ai s1 s2 step)
  else if s1.status = 1 then
    (moveto 10 100;
     draw_string "PLAYER 1 HAS WON";
     repl_vs_ai s1 s2 step)
  else if s2.status = 1 then
    (moveto 250 100;
     draw_string "THE AI WON";
     repl_vs_ai s1 s2 step)
  else if s1.status = -1 then
    (moveto 40 120;
    draw_string "PLAYER 1 CANNOT MOVE";
     moveto 180 80; draw_string "THE AI WON";
     repl_vs_ai s1 s2 step)
  else if s2.status = -1 then
    (moveto 280 130;
     draw_string "THE AI CANNOT MOVE";
     moveto 200 80; draw_string "YOU WON";
     repl_vs_ai s1 s2 step)
  else
    let e = wait_next_event [Key_pressed] in
    match e.key with
    | 'w' | 's' | 'a'| 'd' ->
    let newstate = update_state (parse_command e.key) s1 in
    ai_move s2 step newstate
    | _ -> repl_vs_ai s2 s2 step
and player1vsai_move newstate newstate_ai step=
  clear_graph ();
  draw_board 250 300 newstate_ai; draw_board 10 300 newstate;
  repl_vs_ai newstate newstate_ai step
and ai_move s2 step newstate =
  let best_move = get_best_move s2 step in
  let newstate_ai = update_state best_move s2 in
  player1vsai_move newstate newstate_ai step

let rec play_game s1 s2 =
  open_graph " 480x480";
  moveto 150 300;
  draw_string "Welcome to 2048!!!!!!";
  moveto 125 280;
  draw_string "Press '1' to play one player";
  moveto 125 260;
  draw_string "Press '2' to play multiplayer";
  moveto 125 240;
  draw_string "Press 'a' to see the AI solve 2048";
  moveto 40 220;
  draw_string
    "Press 'c' to challenge yourself and beat our AI with less moves!";
  moveto 50 200;
  draw_string "Click the 'x' on the left, top corner of the window to quit";
  moveto 70 120;
  draw_string "Player 1 controls: 'wasd' for up, left, down, right";
  moveto 70 100;
  draw_string "Player 2 controls: 'ijkl' for up, left, down, right";
  let e = wait_next_event [Key_pressed] in
  match e.key with
  | '1' ->
    clear_graph ();
    draw_board 120 300 s1;
    repl_1player s1
  | '2' ->
    clear_graph ();
    draw_2board s1 s2;
    repl_2player s1 s2
  | 'a' ->
    clear_graph ();
    draw_board 120 300 s1;
    repl_ai s1
  | 'c' ->
    clear_graph ();
    vs_ai_screen s1 s2;
  | _ -> play_game s1 s2
and vs_ai_screen s1 s2 =
  moveto 170 300;
  draw_string "CHOOSE DIFFICULTY";
  moveto 170 280;
  draw_string "Press 'e' for Easy";
  moveto 150 260;
  draw_string "Press 'i' for Intermediate";
  moveto 160 240;
  draw_string "Press 'd' for Difficult";
  moveto 130 200;
  draw_string "Press 'b' to go back to the home screen";
  let e = wait_next_event [Key_pressed] in
  match e.key with
  | 'e' ->
    clear_graph ();
    draw_2board s1 s2;
    repl_vs_ai s1 s2 4
  | 'i' ->
    clear_graph ();
    draw_2board s1 s2;
    repl_vs_ai s1 s2 5
  | 'd' ->
    clear_graph ();
    draw_2board s1 s2;
    repl_vs_ai s1 s2 6
  | 'b' -> play_game s1 s2
  | _ -> clear_graph (); vs_ai_screen s1 s2

(* [main] runs the game *)
let main () =
 let state1 = State.init_state in
 let state2 = State.init_state in
 try
   play_game state1 state2
 with
 | _ -> print_endline("You have quitted the game")

let () = main ()
