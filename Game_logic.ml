open Caml_types

let cols       = 10
let rows_count = 10

(* player movement, match against what input was recieved and change position based on delta column delta row *)
let move_player (col, row) dir =
  let (dc, dr) = match dir with
    | Up    -> ( 0, -1)
    | Down  -> ( 0,  1)
    | Left  -> (-1,  0)
    | Right -> ( 1,  0)
  in
  let col' = max 0 (min (cols - 1) (col + dc)) in
  let row' = max 0 (min (rows_count - 1) (row + dr)) in
  (col', row')

let apply_input dir_opt state =
  match dir_opt with
  | Some d -> { state with player_pos = move_player state.player_pos d }
  | None   -> state

(* initial state, initially set but can randomize + generate later*)

let initial_state = {
  player_pos = (cols / 2, rows_count - 1);
  rows = [
    { kind = Grass; obstacles = [] };
    { kind = Road;  obstacles = [{ pos = (2, 1); kind = Car;   facing = Right }] };
    { kind = Road;  obstacles = [{ pos = (6, 2); kind = Truck; facing = Left  }] };
    { kind = Water; obstacles = [{ pos = (1, 3); kind = Log;   facing = Right }] };
    { kind = Water; obstacles = [{ pos = (5, 4); kind = Log;   facing = Right }] };
    { kind = Grass; obstacles = [] };
    { kind = Road;  obstacles = [{ pos = (3, 6); kind = Car;   facing = Left  }] };
    { kind = Road;  obstacles = [{ pos = (7, 7); kind = Car;   facing = Right }] };
    { kind = Water; obstacles = [{ pos = (0, 8); kind = Log;   facing = Right }] };
    { kind = Grass; obstacles = [] };
  ];
  score        = 0;
  is_game_over = false;
}

let move_obstacles state =
  let (p_col, p_row) = state.player_pos in
  let new_player_col = ref p_col in

  let rows' = List.mapi (fun r_idx row ->
    let obs' = List.map (fun obs ->
      let (col, _) = obs.pos in
      let col' = match obs.facing with
        | Right -> (col + 1) mod cols
        | Left  -> (col - 1 + cols) mod cols
        | _     -> col
      in
      (* If player is on this obstacle and it's a Water row (a Log), update player position *)
      if r_idx = p_row && col = p_col && row.kind = Water then
        new_player_col := col';

      { obs with pos = (col', r_idx) }
    ) row.obstacles in
    { row with obstacles = obs' }
  ) state.rows in
  { state with rows = rows'; player_pos = (!new_player_col, p_row) }

(*Check for player collison*)
let check_collision state =
  let (_p_col, p_row_idx) = state.player_pos in
  let current_row = List.nth state.rows p_row_idx in
  let on_obstacle = List.exists (fun obs -> obs.pos = state.player_pos) current_row.obstacles in

  let should_die = match current_row.kind with
    | Road  -> on_obstacle
    | Water -> not on_obstacle
    | Grass -> false
  in
  if should_die then { state with is_game_over = true } else state

let check_goal state =
  let goal_pos = List.init cols (fun x -> (x, 0)) in 
  let reached_end = List.exists (fun pos -> pos = state.player_pos) goal_pos in

    if reached_end then 
      { state with
        score = state.score + 1;
        player_pos = (cols / 2, rows_count - 1)
      }
    else
    state


