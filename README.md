# Crossy Caml
Charlie Landman and Eli Stolte

A functional Frogger/Crossy Road clone implemented in OCaml.

## Overview
Crossy Caml features a procedurally generated grid-based world where the player must navigate through grass, roads, and water. The game uses a pure functional state-transformation model to handle movement, collisions, and log-riding mechanics.

## Requirements
To build and run this project, you will need:
- **OCaml** (4.12+)
- **Dune** build system
- **Graphics** library: `opam install graphics`
- **Unix** library (included with standard OCaml distributions)

## Commands
- **Build the project:** `dune build`
- **Run the game:** `dune exec ./main.exe`
- **Clean build artifacts:** `dune clean`


### Game_logic.ml
- `List.init`: Used in `generate_map` and `generate_row` to procedurally construct the grid and obstacle sets.
- `List.map`: Utilized to transform lists of obstacles during state updates.
- `List.mapi`: Used in `move_obstacles` to iterate over rows with their index, enabling row-specific logic like alternating directions and allowing player and log to exist on the same tile.
- `List.exists`: Powering `check_collision` and `check_goal` by searching for specific coordinate matches within obstacle and terrain lists.
- `|>` (Pipeline): Used for clean, readable coordinate calculations in `move_player`.

### main.ml
- **Pipeline Operator (`|>`):** Heavily used in the main `loop` to chain state transformations, passing the immutable game state through input handling, movement logic, and collision checks.

## Disclaimer
The GUI and rendering logic (`Render.ml`) were developed with the assistance of **Claude Code**.

## Note on Game Over
There is currently no in-game graphical "Game Over" screen. When a collision or drowning occurs, the game window will close, and your final score will be output directly to the CLI.
