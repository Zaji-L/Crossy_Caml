open Caml_types

let cols       = 10
let rows_count = 10

(* move_player : position -> direction -> position
   Calculates new player position based on direction while staying within grid bounds. *)
let move_player (col, row) dir =
  let (dc, dr) = match dir with
    | Up    -> ( 0, -1)
    | Down  -> ( 0,  1)
    | Left  -> (-1,  0)
    | Right -> ( 1,  0)
  in
  let col' = col + dc |> min (cols - 1) |> max 0 in
  let row' = row + dr |> min (rows_count - 1) |> max 0 in
  (col', row')

(* apply_input : direction option -> gamestate -> gamestate
   Updates the game state with the player's new position if an input direction is provided. *)
let apply_input dir_opt state =
  match dir_opt with
  | Some d -> { state with player_pos = move_player state.player_pos d }
  | None   -> state

(* generate_row : int -> row
   Produces a randomized terrain row with obstacles based on the row index. *)
let generate_row r_idx =
  if r_idx = 0 || r_idx = (rows_count - 1) then
    { kind = Grass; obstacles = [] }
  else
    let terrain_roll = Random.int 10 in
    let kind =
      if terrain_roll < 2 then Grass
      else if terrain_roll < 6 then Road
      else Water
    in
    (* Solvability rule: Alternating directions based on row index *)
    let dir = if r_idx mod 2 = 0 then Right else Left in
    let obstacles = match kind with
      | Grass -> []
      | Road ->
          (* Density rule: 2-3 cars per road *)
          let count = 2 + Random.int 2 in
          List.init count (fun i ->
            { pos = ((i * (cols / count)) + Random.int (cols / count), r_idx);
              kind = (if Random.bool () then Car else Truck);
              facing = dir }
          )
      | Water ->
          (* Density rule: 3-4 logs per water row to ensure overlap *)
          let count = 3 + Random.int 2 in
          List.init count (fun i ->
            { pos = ((i * (cols / count)) + Random.int (cols / count), r_idx);
              kind = Log;
              facing = dir }
          )
    in
    { kind; obstacles }

(* generate_map : unit -> row list
   Generates a full map consisting of rows_count number of rows. *)
let generate_map () =
  List.init rows_count generate_row

(* initial_state : unit -> gamestate
   Creates the starting game state with a fresh map and default player position. *)
let initial_state () = {
  player_pos = (cols / 2, rows_count - 1);
  rows = generate_map ();
  score        = 0;
  is_game_over = false;
}

(* move_obstacles : gamestate -> gamestate
   Updates the positions of all obstacles and moves the player if they are riding a log. *)
let move_obstacles state =
  let (p_col, p_row) = state.player_pos in
  let new_player_col = ref p_col in

  let rows' = 
    state.rows 
    |> List.mapi (fun r_idx row ->
      let obs' = 
        row.obstacles 
        |> List.map (fun obs ->
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
        ) 
      in
      { row with obstacles = obs' }
    ) 
  in
  { state with rows = rows'; player_pos = (!new_player_col, p_row) }

(* check_collision : gamestate -> gamestate
   Determines if the player has collided with a hazard or drowned in water. *)
let check_collision state =
  let (_p_col, p_row_idx) = state.player_pos in
  let current_row = List.nth state.rows p_row_idx in
  let on_obstacle = 
    current_row.obstacles 
    |> List.exists (fun obs -> obs.pos = state.player_pos) 
  in

  let should_die = match current_row.kind with
    | Road  -> on_obstacle
    | Water -> not on_obstacle
    | Grass -> false
  in
  if should_die then { state with is_game_over = true } else state

(* check_goal : gamestate -> gamestate
   Checks if the player reached the top row and resets position with a new map if successful. *)
let check_goal state =
  let reached_end = 
    List.init cols (fun x -> (x, 0))
    |> List.exists (fun pos -> pos = state.player_pos) 
  in

  if reached_end then 
    { state with
      score = state.score + 1;
      player_pos = (cols / 2, rows_count - 1);
      rows = generate_map ()
    }
  else
    state
