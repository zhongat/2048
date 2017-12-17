open Graphics
open State

(* [tile_color] is a type that defines the color of a tile depending on its
 * value. *)
type tile_color =
  | Red
  | Orange
  | Yellow
  | LightGreen
  | Green
  | Teal
  | Sky
  | Blue
  | Indigo
  | Pink
  | Gray

(* [pick_tile_color n] determines the [tile_color] of a tile given its value
 * [n].
 * requires: [n] is a valid value (2 <= n <= 2048), and n = 2 ^ x for some
 * x *)
let pick_tile_color = function
  | 2 -> Red
  | 4 -> Orange
  | 8 -> Yellow
  | 16 -> LightGreen
  | 32 -> Green
  | 64 -> Teal
  | 128 -> Sky
  | 256 -> Blue
  | 512 -> Indigo
  | 1024 -> Pink
  | 2048 -> Gray
  | _ -> failwith "Not a valid value"

(* [generate_tile_color c] takes in the tile color [c] and returns its RGB
 * value.
 * requires: [c] is a tile color *)
let generate_tile_color = function
  | Red -> rgb 255 153 153
  | Orange -> rgb 255 204 153
  | Yellow -> rgb 255 255 153
  | LightGreen -> rgb 204 255 153
  | Green -> rgb 153 255 153
  | Teal -> rgb 0 204 204
  | Sky -> rgb 153 153 255
  | Blue -> rgb 153 204 255
  | Indigo -> rgb 204 153 255
  | Pink -> rgb 255 204 229
  | Gray -> rgb 224 224 224

(* [draw_grid x y w h] draws the 4x4 grid map of the board.
 * [x] is the x-coordinate position of the grid
 * [y] is the y-coordinate position of the grid
 * [w] is the width of each grid box
 * [h] is the height of each grid box
 * requires: [x], [y], [w], [h] are integers *)
let draw_grid x y w h =
  (* [draw_row x y w h n] draws a row [n] times
   * [x] is the x-coordinate of the row
   * [y] is the y-coordinate of the row
   * [w] is the width of each grid box in the row
   * [h] is the height of each grid box in the row
   * [n] is the number of rows to draw *)
  let rec draw_row x y w h n =
    match n with
    | 0 -> ()
    | n ->
      draw_rect x y w h;
      draw_row (x + w) y w h (n - 1) in
  let rec loop x y w h n =
    match n with
    | 0 -> ()
    | n ->
      draw_row x y w h 4;
      loop x (y - h) w h (n - 1) in
  loop x y w h 4

(* [draw_tiles xpos ypos tlist w h] draws each tile of the game given
 * the [tlist], which is a list of integers indexed at the position they
 * should be drawn on the board.
 *
 *  | 0 | 1 | 2 | 3 |
 *  | 4 | 5 | 6 | 7 |
 *  | 8 | 9 | 10| 11|
 *  | 12| 13| 14| 15|
 *
 * represents the position that each value should be
 * requires: [xpos] is a valid int
 * [ypos] is a valid int
 * [tlist] is a valid list of ints, with length 16
 * [w] is a valid int
 * [h] is a valid height *)
let draw_tiles xpos ypos tlist w h =
  for i = 0 to 15 do
    let newx = xpos + ((i mod 4) * w) in
    let newy = ypos - (((i/4)) * h) in
    if (List.nth tlist i) <> 0
    then
      let c = pick_tile_color (List.nth tlist i) in
      set_color (generate_tile_color c);
      fill_rect newx newy w h;
      set_color black;
      draw_grid xpos ypos w h;
      let draw_string_in_tile x y w value =
        let string_value = string_of_int value in
        if value > 1000 then
        (moveto ((x + w/2)-9) ((y + w/2)-3);
        draw_string string_value)
        else if value > 100 then
        (moveto ((x + w/2)-6) ((y + w/2)-3);
        draw_string string_value)
        else
        (moveto ((x + w/2)-3) ((y + w/2)-3);
        draw_string string_value) in
      draw_string_in_tile newx newy w (List.nth tlist i)
    else ()
  done


let draw_board xpos ypos st =
  draw_grid xpos ypos 50 50;
  draw_tiles xpos ypos st.board 50 50;
