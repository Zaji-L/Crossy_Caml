open Caml_types

let tile_size  = 64
let cols       = 10
let rows_count = 10

(* ── Colour helpers ─────────────────────────────────────────────────────── *)

let set_color_terrain = function
  | Road  -> Graphics.set_color (Graphics.rgb 80  80  80)
  | Grass -> Graphics.set_color (Graphics.rgb 34 139  34)
  | Water -> Graphics.set_color (Graphics.rgb 30 144 255)

let set_color_obstacle = function
  | Car   -> Graphics.set_color (Graphics.rgb 220  20  60)
  | Truck -> Graphics.set_color (Graphics.rgb 255 140   0)
  | Log   -> Graphics.set_color (Graphics.rgb 139  90  43)

(* ── Drawing helpers ────────────────────────────────────────────────────── *)

(* Graphics origin is bottom-left; row 0 is the bottom of the screen *)
let tile_x col = col * tile_size
let tile_y row = (rows_count - 1 - row) * tile_size

let draw_tile terrain row col =
  set_color_terrain terrain;
  Graphics.fill_rect (tile_x col) (tile_y row) tile_size tile_size;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect (tile_x col) (tile_y row) tile_size tile_size

let draw_obstacle obs =
  let (col, row) = obs.pos in
  set_color_obstacle obs.kind;
  let w = match obs.kind with Truck -> tile_size * 2 | _ -> tile_size in
  Graphics.fill_rect (tile_x col) (tile_y row) w tile_size

let draw_player (col, row) =
  Graphics.set_color (Graphics.rgb 0 200 0);
  let margin = 8 in
  Graphics.fill_rect
    (tile_x col + margin) (tile_y row + margin)
    (tile_size - margin * 2) (tile_size - margin * 2)

(* ── Render ─────────────────────────────────────────────────────────────── *)

let render state =
  Graphics.clear_graph ();
  List.iteri (fun r row ->
    for c = 0 to cols - 1 do
      draw_tile row.kind r c
    done;
    List.iter draw_obstacle row.obstacles
  ) state.rows;
  draw_player state.player_pos;
  Graphics.set_color Graphics.white;
  Graphics.moveto 4 4;
  Graphics.draw_string (Printf.sprintf "Score: %d" state.score);
  Graphics.synchronize ()
