open State
open Board
open Command
open OUnit2

let b1 = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;2]
let st1 = {score = 0; board = b1; status = 0}
let up_st1 = update_state Up1 st1
let down_st1 = update_state Down1 st1
let left_st1 = update_state Left1 st1
let right_st1 = update_state Right1 st1

let b2 = [64;16;8;2;2;16;4;2;0;0;4;2;0;0;0;2]
let st2 = {score = 122; board = b2; status = 0}
let up_st2 = update_state Up1 st2
let down_st2 = update_state Down1 st2
let left_st2 = update_state Left1 st2
let right_st2 = update_state Right1 st2

let b3 = [1024;1024;64;128;2;16;8;4;64;32;2;2;2;2;4;8]
let st3 = {score = 2908; board = b3; status = 0}
let up_st3 = update_state Up1 st3
let down_st3 = update_state Down1 st3
let left_st3 = update_state Left1 st3
let right_st3 = update_state Right1 st3

let b4 = [256;512;128;8;16;32;2;512;64;1024;2;128;2;32;16;8]
let st4 = {score = 1500; board = b4; status = 0}
let up_st4 = update_state Up1 st4
let down_st4 = update_state Down1 st4
let left_st4 = update_state Left1 st4
let right_st4 = update_state Right1 st4

(* [compare b1 b2] compares the board [b1] and board [b2] and returns true if
 * there is only one discrepancy between the tiles of the two boards and
 * false otherwise. *)
let compare b1 b2 =
  let rec helper first second acc=
  match first, second with
  | [],[] -> acc
  | h1::t1,h2::t2 ->
    if h1 <> h2 then helper t1 t2 (acc+1)
    else helper t1 t2 acc
  | _ -> failwith "error" in
  ((helper b1 b2 0)<2)

let tests =
[
  (*******************************************************************)
  (* Tests for up_move *)
  (*******************************************************************)
  "st1_up" >:: (fun _ -> assert_equal true
    (compare (get_board up_st1) [0;0;0;2;0;0;0;0;0;0;0;0;0;0;0;0]));
  "st1_up_wrong" >:: (fun _ -> assert_equal false
    (compare (get_board up_st1) [0;0;0;2;0;0;0;0;0;0;0;0;8;0;0;2]));
  "st2_up" >:: (fun _ -> assert_equal true
    (compare (get_board up_st2) [64;32;8;4;2;0;8;4;0;0;0;0;0;0;0;0]));
  "st3_up" >:: (fun _ -> assert_equal true
    (compare (get_board up_st3)
    [1024;1024;64;128;2;16;8;4;64;32;2;2;2;2;4;8]));
  "st4_up" >:: (fun _ -> assert_equal true
    (compare (get_board up_st4)
    [256;512;128;8;16;32;4;512;64;1024;16;128;2;32;0;8]));

  (*******************************************************************)
  (* Tests for down_move *)
  (*******************************************************************)
  "st1_down" >:: (fun _ -> assert_equal true
    (compare (get_board down_st1) [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;2]));
  "st1_down_wrong" >:: (fun _ -> assert_equal false
    (compare (get_board down_st1) [0;0;0;2;0;0;0;0;0;0;0;0;0;2;0;0]));
  "st2_down" >:: (fun _ -> assert_equal true
    (compare (get_board down_st2) [0;0;0;0;0;0;0;0;64;0;8;4;2;32;8;4;]));
  "st3_down" >:: (fun _ -> assert_equal true
    (compare (get_board down_st3)
    [1024;1024;64;128;2;16;8;4;64;32;2;2;2;2;4;8]));
  "st4_down" >:: (fun _ -> assert_equal true
    (compare (get_board down_st4)
    [256;512;0;8;16;32;128;512;64;1024;4;128;2;32;16;8]));

  (*******************************************************************)
  (* Tests for right_move *)
  (*******************************************************************)
  "st1_right" >:: (fun _ -> assert_equal true
    (compare (get_board right_st1) [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;2]));
  "st1_right_wrong" >:: (fun _ -> assert_equal false
    (compare (get_board right_st1) [0;0;0;2;0;0;0;0;0;0;0;0;0;4;0;0]));
  "st2_right" >:: (fun _ -> assert_equal true
    (compare (get_board right_st2) [64;16;8;2;2;16;4;2;0;0;4;2;0;0;0;2]));
  "st3_right" >:: (fun _ -> assert_equal true
    (compare (get_board right_st3) [0;2048;64;128;2;16;8;4;0;64;32;4;0;4;4;8]));
  "st4_right" >:: (fun _ -> assert_equal true
    (compare (get_board right_st4)
    [256;512;128;8;16;32;2;512;64;1024;2;128;2;32;16;8]));

  (*******************************************************************)
  (* Tests for left_move *)
  (*******************************************************************)
  "st1_right" >:: (fun _ -> assert_equal true
    (compare (get_board left_st1) [0;0;0;0;0;0;0;0;0;0;0;0;2;0;0;0]));
  "st1_right_wrong" >:: (fun _ -> assert_equal false
    (compare (get_board left_st1) [0;0;0;2;0;0;0;0;0;0;0;0;0;4;0;0]));
  "st2_right" >:: (fun _ -> assert_equal true
    (compare (get_board left_st2) [64;16;8;2;2;16;4;2;4;2;0;0;2;0;0;0]));
  "st3_right" >:: (fun _ -> assert_equal true
    (compare (get_board left_st3) [2048;64;128;0;2;16;8;4;64;32;4;0;4;4;8;0]));
  "st4_right" >:: (fun _ -> assert_equal true
    (compare (get_board left_st4)
    [256;512;128;8;16;32;2;512;64;1024;2;128;2;32;16;8]));


  (*******************************************************************)
  (* Tests for get_score *)
  (*******************************************************************)
  "st1_score" >:: (fun _ -> assert_equal 0 (get_score st1));
  "up_st1_score" >:: (fun _ -> assert_equal 0 (get_score up_st1));
  "down_st1_score" >:: (fun _ -> assert_equal 0 (get_score down_st1));
  "left_st1_score" >:: (fun _ -> assert_equal 0 (get_score left_st1 ));
  "right_st1_score" >:: (fun _ -> assert_equal 0 (get_score right_st1));

  "st2_score" >:: (fun _ -> assert_equal 122 (get_score st2));
  "up_st2_score" >:: (fun _ -> assert_equal 170 (get_score up_st2));
  "down_st2_score" >:: (fun _ -> assert_equal 170 (get_score down_st2));
  "left_st2_score" >:: (fun _ -> assert_equal 122 (get_score left_st2));
  "right_st2_score" >:: (fun _ -> assert_equal 122 (get_score right_st2));

  "st3_score" >:: (fun _ -> assert_equal 2908 (get_score st3));
  "up_st3_score" >:: (fun _ -> assert_equal 2908 (get_score up_st3));
  "down_st3_score" >:: (fun _ -> assert_equal 2908 (get_score down_st3));
  "left_st3_score" >:: (fun _ -> assert_equal 4964 (get_score left_st3));
  "right_st3_score" >:: (fun _ -> assert_equal 4964 (get_score right_st3));

  "st4_score" >:: (fun _ -> assert_equal 1500 (get_score st4));
  "up_st4_score" >:: (fun _ -> assert_equal 1504 (get_score up_st4));
  "down_st4_score" >:: (fun _ -> assert_equal 1504 (get_score down_st4));
  "left_st4_score" >:: (fun _ -> assert_equal 1500 (get_score left_st4));
  "right_st4_score" >:: (fun _ -> assert_equal 1500 (get_score right_st4));

  (*******************************************************************)
  (* Tests for get_status *)
  (*******************************************************************)
  "st1_status" >:: (fun _ -> assert_equal 0 (get_status st1));
  "up_st1_status" >:: (fun _ -> assert_equal 0 (get_status up_st1));
  "down_st1_status" >:: (fun _ -> assert_equal 0 (get_status down_st1));
  "left_st1_status" >:: (fun _ -> assert_equal 0 (get_status left_st1));
  "right_st1_status" >:: (fun _ -> assert_equal 0 (get_status right_st1));

  "st2_status" >:: (fun _ -> assert_equal 0 (get_status st2));
  "up_st2_status" >:: (fun _ -> assert_equal 0 (get_status up_st2));
  "down_st2_status" >:: (fun _ -> assert_equal 0 (get_status down_st2));
  "left_st2_status" >:: (fun _ -> assert_equal 0 (get_status left_st2));
  "right_st2_status" >:: (fun _ -> assert_equal 0 (get_status right_st2));

  "st3_status" >:: (fun _ -> assert_equal 0 (get_status st3));
  "up_st3_status" >:: (fun _ -> assert_equal 0 (get_status up_st3));
  "down_st3_status" >:: (fun _ -> assert_equal 0 (get_status down_st3));
  "left_st3_status" >:: (fun _ -> assert_equal 1 (get_status left_st3));
  "right_st3_status" >:: (fun _ -> assert_equal 1 (get_status right_st3));

  "st4_status" >:: (fun _ -> assert_equal 0 (get_status st4));
  "up_st4_status" >:: (fun _ -> assert_equal (-1) (get_status up_st4));
  "down_st4status" >:: (fun _ -> assert_equal (-1) (get_status down_st4));
  "left_st4_status" >:: (fun _ -> assert_equal 0 (get_status left_st4));
  "right_st4_status" >:: (fun _ -> assert_equal 0 (get_status right_st4));
]

let suite =
  "Test suite"
  >::: tests

let _ = run_test_tt_main suite
