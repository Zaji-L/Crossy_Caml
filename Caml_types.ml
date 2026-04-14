(* Represents the 2D coordinate system for the player and objects *)
type position = int * int

(* Directions the player can move; also defines movement for obstacles *)
type direction = Up | Down | Left | Right

(* Different types of obstacles with varying widths or behaviors *)
type obstacle_type = Car | Truck | Log

(* An obstacle has a position, a type, and a direction it is facing/moving *)
type obstacle = {
  pos : position;
  kind : obstacle_type;
  facing : direction;
}

(* Defines the background of a specific row *)
type terrain = Road | Grass | Water

(* A row contains its terrain type and a list of obstacles currently in it *)
type row = {
  kind : terrain;
  obstacles : obstacle list;
}

(* The game state tracks the player's progress, their current position, 
   and the map generated so far *)
type gamestate = {
  player_pos : position;
  rows : row list;         (* The world map *)
  score : int;             (* Number of roads cleared *)
  is_game_over : bool;
}
