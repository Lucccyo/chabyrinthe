type wall = {
  mutable st : bool;
  dir: | N | S | E | W;
  c_index: int;
}

let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)

let display grid = ...

let rand_wall wall_arr = 
  Random.self_init ();
  let r = Random.int (Array.length wall_arr) in
  wall_arr.(r)

let rand_cell grid = 
  Random.self_init ();
  let rand_cell = Random.int (Array.length grid) in
  grid.(rand_cell)

let rec process grid wall_arr = 
  if Array.length = 0
  then ()
  else 
    Random.self_init ();
    let r = Random.int (Array.length wall_arr) in
    let rand_wall = wall_arr.(r) in 
    let n = match rand_wall.orientation with
      | N -> (r - l - 1) 
      | S -> (r + l - 1) 
      | E -> (r + 1) 
      | W -> (r - 1) 
    process grid Array.append (Array.append (Array.sub wall_arr 0 r) (Array.sub wall_arr (r + 1) ((Array.length wall_arr) - r -1))) grid (n) 

let () =
    (* ajouter l'index de la cellule y*)
  let init_grid = Array.make (l * h)
    ([|{st = true; orientation = N};
       {st = true; orientation = S}; 
       {st = true; orientation = E}; 
       {st = true; orientation = W} |]) in
  process (rand_cell init_grid) in




(* test *)
(* [|
  [|"―――";"  |"; "___"; "|  "|];
  [|"―――";"  |"; "___"; "|  "|];
  [|"―――";"  |"; "___"; "|  "|];
  [|"―――";"  |"; "___"; "|  "|];|] *)