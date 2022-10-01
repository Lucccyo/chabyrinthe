type cell = {
  south: bool;
  east : bool;
}

let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)

let print_cell c = 
  if c.south
  then Format.printf "___"
  else Format.printf "  |"

let rec print_grid grid c l h = 
  if c >= (h * l)
  then ()
  else (let line = Array.sub grid c l in
    Array.iter (fun c -> print_cell c) line;
    Format.printf "\n";
    print_grid grid (c+l) l h)

let display grid = print_grid grid 0 l h 

let grid = Array.make (l * h) ({south = true ; east = true})

let maze = Array.map (fun c -> 
  Random.self_init ();
  if (Random.int 100) mod 2 = 0 
  then {south = false   ; east = c.east} 
  else {south = c.south ; east = false}) grid |> display
