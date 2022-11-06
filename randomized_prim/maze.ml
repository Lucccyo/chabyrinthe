open Format 

type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}

let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)

let display grid = 
  for i = 0 to (Array.length grid) - 1 do
    if i mod l = 0 then printf "\n";
    if grid.(i).ws then printf "___" else printf "   ";
    if grid.(i).we then printf "|" else printf " ";
  done;
  printf "\n%!"

(* val add : wall array * int -> wall -> wall array * int *)
let add (wrapper : wall array * int) w =
  let l   = snd wrapper in
  let wip = fst wrapper in
  if Array.mem w wip
  then wrapper
  else ( 
    wip.(l) <- w; 
    wip, l + 1)
  
(* val remove : wall array * int -> int -> wall array * int *)
let remove (wrapper : wall array * int) r = 
  let l   = snd wrapper in
  let wip = fst wrapper in
  wip.(r) <- wip.(l - 1); 
  wip, l - 1

(* val is_not_visited : cell array -> cell -> int -> bool *)
let is_not_visited grid c i = 
  let wn = try (grid.(i - l)).ws with Invalid_argument _ -> true in
  let ww = try (grid.(i - 1)).we with Invalid_argument _ -> true in
  c.we && c.ws && wn && ww

(* val get_walls_from_cell : wall -> wall list  *)
let get_walls_from_cell w = 
  let l = 
    [if w.e_or_s 
    then {x = w.x; y = w.y; e_or_s = false} (* south *)
    else {x = w.x; y = w.y; e_or_s = true}] (* east  *) in 
  let l = l @ (if w.x > 0 then [{x = w.x - 1; y = w.y; e_or_s = true }] else []) in (* west *)
          l @ (if w.y > 0 then [{x = w.x; y = w.y - 1; e_or_s = false}] else [])    (* north *)

(* val get_walls_from_neighbour : wall -> wall list  *)
let get_walls_from_neighbour w = 
  match w.e_or_s with
  | true  -> 
    let l = 
      if w.x < l
      then [{x = w.x + 1; y = w.y; e_or_s = true};  (* east *)
            {x = w.x + 1; y = w.y; e_or_s = false}] (* south *)
      else [] in 
    l @ (if w.y > 0 then [{x = w.x + 1; y = w.y - 1; e_or_s = false}] else []) (* north *)
  | false -> 
    let l = 
      if w.y < h
      then [{x = w.x; y = w.y + 1; e_or_s = true};  (* east *)
            {x = w.x; y = w.y + 1; e_or_s = false}] (* south *)
      else [] in 
    l @ (if w.x > 0 then [{x = w.x - 1; y = w.y + 1; e_or_s = true}] else []) (* east *)

(* val break : wall -> cell array -> int -> wall array * int -> wall array * int *)
let break w grid r wrapper =
  let i = w.y * l + w.x in
  let i_not_visited = is_not_visited grid grid.(i) i in
  let j = if w.e_or_s then i + 1 else i + l in
  let j_not_visited =
    (try
      if j = i + 1 && (i mod l = l - 1) 
      then false 
      else is_not_visited grid (grid.(j)) j 
    with Invalid_argument _ -> false) in
  if not (i_not_visited || j_not_visited)
  then remove wrapper r
  else begin
    grid.(i) <- if w.e_or_s
      then {we = false ; ws = grid.(i).ws}
      else {we = grid.(i).we ;ws = false};
    let li = 
      if not i_not_visited 
      then []
      else get_walls_from_cell w in
    let lj = 
      if not j_not_visited
      then []
      else get_walls_from_neighbour w in
    let rec add_lst l wrapper = 
      match l with 
      | [] -> wrapper
      | hd :: tl -> add_lst tl (add wrapper hd) in
    remove (add_lst (li @ lj) wrapper) r end

(* val progress : wall array * int -> cell array -> cell array *)
let rec progress (wrapper : wall array * int) grid = 
  if snd wrapper = 0
  then grid
  else  
    let r = Random.int (snd wrapper) in
    let next_w_arr = break (fst wrapper).(r) grid r wrapper in
    progress next_w_arr grid

(* val init_walls : int -> wall array -> int -> int -> wall array *)
let rec init_walls i walls col line = 
  if i = Array.length walls
  then walls
  else (
    walls.(i)     <- {x = col; y = line; e_or_s = true};
    walls.(i + 1) <- {x = col; y = line; e_or_s = false};
    let new_col  = (col + 1) mod l in
    let new_line = (if col = new_col - 1 then line else line + 1) in
    init_walls (i+2) walls new_col new_line)

let init_walls () = 
  init_walls  0 (Array.make (l * h * 2) {x = -1; y = -1; e_or_s = true}) 0 0

let maze =
  Random.self_init ();
  let grid  = Array.make (l * h) {we = true; ws =  true} in
  let walls = init_walls () in
  let init_wrapper = (Array.make (l * h * 2) {x = -1; y = -1; e_or_s = true}, 0) in
  let start_arr = add init_wrapper walls.(Random.int (l * h * 2)) in
  display (progress start_arr grid)
