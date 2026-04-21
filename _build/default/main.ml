open Caml_types

let win_w = Render.tile_size * Render.cols
let win_h = Render.tile_size * Render.rows_count

(* wasd input *)

let read_input () =
  if Graphics.key_pressed () then
    match Graphics.read_key () with
    | 'w' | 'W' -> Some Up
    | 's' | 'S' -> Some Down
    | 'a' | 'A' -> Some Left
    | 'd' | 'D' -> Some Right
    | _          -> None
  else None

(* game loop logic *)

let rec loop state =
  if state.is_game_over then ()
  else begin
    Render.render state;
    Unix.sleepf 0.5;
    let state' = Game_logic.apply_input (read_input ()) state in
    let state'' = Game_logic.move_obstacles state' in 
    loop state''
  end

let () =
  Graphics.open_graph (Printf.sprintf " %dx%d" win_w win_h);
  Graphics.set_window_title "Crossy Caml";
  Graphics.auto_synchronize false;
  loop Game_logic.initial_state;
  Graphics.close_graph ()
