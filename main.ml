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
  if state.is_game_over then 
    let () = Printf.printf "Game Over! Final Score: %d\n" state.score in
    Graphics.close_graph ()
  else 
    state
    |> Game_logic.apply_input (read_input ())
    |> Game_logic.move_obstacles
    |> Game_logic.check_collision
    |> Game_logic.check_goal
    |> (fun next_state -> 
         Render.render next_state;
         Unix.sleepf 0.5;
         loop next_state)

let () =
  Random.self_init ();
  Graphics.open_graph (Printf.sprintf " %dx%d" win_w win_h);
  Graphics.set_window_title "Crossy Caml";
  Graphics.auto_synchronize false;
  loop (Game_logic.initial_state ())
