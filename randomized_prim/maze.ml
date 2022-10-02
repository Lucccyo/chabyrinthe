type dir = | N | S | E | W

type wall = {
  mutable st : bool;
  orientation: dir;
}

let length = int_of_string Sys.argv.(1)
let hight  = int_of_string Sys.argv.(2)

let display grid =

let rand_wall wall_arr = 
  Random.self_init ();
  let r = Random.int (Array.length wall_arr) in
  wall_arr.(r)

let rand_cell grid = 
  Random.self_init ();
  let rand_cell = Random.int (Array.length grid) in
  grid.(rand_cell)
 
let rec process wall_arr = 
  if Array.length = 0
  then ()
  else 
    let rand_wall = rand_wall wall_arr in
    match rand_wall.orientation with
    | N -> 
    | S -> 
    | E -> 
    | W -> 

let () =
  let init_grid = Array.make (length * hight)
    ([|{st = true; orientation = N};
      {st = true; orientation = S}; 
      {st = true; orientation = E}; 
      {st = true; orientation = W} |]) in
  process (rand_cell init_grid) in



(* on cree les walls quand on les ajoute dans le tableau *)


