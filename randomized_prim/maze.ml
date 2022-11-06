type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}
type dyn_w = (wall array * int)

let l = 20
let h = 20

let display grid = 
  for i = 0 to (Array.length grid)-1 do
    if i mod l = 0 then Format.printf "\n";
    if grid.(i).ws then Format.printf "___" else Format.printf "   ";
    if grid.(i).we then Format.printf "|" else Format.printf " ";
  done;
  Format.printf "\n%!"

(* val add : dyn_w -> wall -> dyn_w *)
let add (w_arr : wall array * int) wall =
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  if Array.mem wall w_arr
  then (
    (w_arr, l))
  else (w_arr.(l) <- wall;
  (w_arr, l + 1))
  
(* val remove : wall array * int -> int -> dyn_w  *)
let remove (w_arr : wall array * int) r = 
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  w_arr.(r) <- w_arr.(l - 1);
  (w_arr, l - 1)  

(* val is_not_visited : cell array -> cell -> int -> bool *)
let is_not_visited grid c i = 
  let n = i - l in
  let w = i - 1 in
  let wn = try (grid.(n)).ws with Invalid_argument _e -> true in
  let ww = try (grid.(w)).we with Invalid_argument _e -> true in
  c.we && c.ws && wn && ww

(* val get_walls_from_cell : wall array -> wall -> wall list  *)
let get_walls_from_cell w = 
  let l = 
  [if w.e_or_s 
  then {x = w.x; y = w.y; e_or_s = false} (* south *)
  else {x = w.x; y = w.y; e_or_s = true}] (* east *) in 
  let l = l @ (if w.x > 0 then [{x = w.x - 1; y = w.y; e_or_s = true}] else []) in (* west *)
          l @ (if w.y > 0 then [{x = w.x; y = w.y - 1; e_or_s = false}] else []) (* north *)

let get_walls_from_neighbour w = 
    match w.e_or_s with
    | true  -> 
      let l = 
        if w.x < l
        then [{x = w.x + 1; y = w.y; e_or_s = true}; (* east *)
              {x = w.x + 1; y = w.y; e_or_s = false}] (* south *)
        else [] in 
        l @ (if w.y > 0 then [{x = w.x + 1; y = w.y - 1; e_or_s = false}] else []) (* north *)
    | false -> 
      let l = 
        if w.y < h
        then [{x = w.x; y = w.y + 1; e_or_s = true}; (* east *)
              {x = w.x; y = w.y + 1; e_or_s = false}] (* south *)
        else [] in 
        l @ (if w.x > 0 then [{x = w.x - 1; y = w.y + 1; e_or_s = true}] else []) (* east *)

(* val break : wall -> cell array -> int -> dyn_w -> dyn_w *)
let break w grid r w_arr =
  let i = w.y * l + w.x in
  let c_i = grid.(i) in
  let i_not_visited = is_not_visited grid c_i i in
  let j = if w.e_or_s then i + 1 else i + l in
  let j_not_visited =
    (try 
      if j = i + 1 && (i mod l = l - 1) 
      then false 
      else is_not_visited grid (grid.(j)) j 
    with Invalid_argument _ -> false) in
  let breakable = i_not_visited || j_not_visited in
  if not breakable
  then remove w_arr r
  else (
    grid.(i) <- 
    if w.e_or_s
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
    let l = li @ lj in
    (* Format.printf "bou1\n"; *)
    let rec add_lst l w_arr= 
      match l with 
      | [] -> w_arr
      | hd :: tl -> 
        add_lst tl (add w_arr hd) in
    remove (add_lst l w_arr) r)

(* val progress : dyn_w -> cell array -> cell array *)
let rec progress (w_arr : wall array * int) grid = 
  if snd w_arr = 0
  then grid
  else  
    let r = Random.int (snd w_arr) in
    let next_w_arr = break (fst w_arr).(r) grid r w_arr in
    progress next_w_arr grid

let rec init_walls i walls col line = 
  if i = Array.length walls
  then walls
  else (
    walls.(i)   <- {x = col; y = line; e_or_s = true};
    walls.(i+1) <- {x = col; y = line; e_or_s = false};
    let new_col  = (col+1) mod l in
    let new_line = (if col = new_col - 1 then line else line + 1) in
    init_walls (i+2) walls new_col new_line)

let init =
  let grid  = Array.make (l * h) {we = true; ws =  true} in
  let walls = Array.make (l * h * 2) {x = -1; y = -1; e_or_s = true} in
  let walls = init_walls 0 walls 0 0 in
  let init_arr = (Array.make (l * h * 2) {x = -1; y = -1; e_or_s = true}, 0) in
  let r = Random.int (l * h * 2) in
  let start_arr = add init_arr walls.(r) in
  display (progress start_arr grid)




