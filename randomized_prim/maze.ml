open Format 

type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}

let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)

let display grid = 
  for i = 0 to l - 1 do printf "___" done;
  printf "_";
  for i = 0 to (Array.length grid) - 1 do
    if i mod l = 0 then printf "\n|";
    if grid.(i).ws then printf "__" else printf "  ";
    if grid.(i).we 
    then printf "|" 
    else printf  
      (if grid.(i).ws || grid.(i + 1).ws
      then "_"
      else " ")
  done;
  printf "\n%!"

(* val add : wall array * int -> wall -> wall array * int *)
let add : wall array * int -> wall -> wall array * int = fun (wip,l) w ->
(* let add (wrapper : wall array * int) w = *)
(*
  let l   = snd wrapper in
  let wip = fst wrapper in
  *)
  wip.(l) <- w; 
  wip, l + 1
  
(* val remove : wall array * int -> int -> wall array * int *)
let remove ((wip, l) : wall array * int) r = 
  (* let l   = snd wrapper in
  let wip = fst wrapper in *)
  wip.(r) <- wip.(l - 1); 
  wip, l - 1

(* val is_not_visited : cell array -> cell -> int -> bool *)
let is_not_visited grid i =
  let c = grid.(i) in 
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

(* val break : cell array -> int -> wall array * int -> wall array * int *)
let break grid r wrapper =
  let w = (fst wrapper).(r) in
  let i = w.y * l + w.x in
  let i_not_visited = is_not_visited grid i in
  let j = if w.e_or_s then i + 1 else i + l in
  let j_not_visited =
    (try
      if j = i + 1 && (i mod l = l - 1) 
      then false 
      else is_not_visited grid j 
    with Invalid_argument _ -> false) in
  let wrapper = remove wrapper r in
  if not (i_not_visited || j_not_visited)
  then wrapper else begin    
    grid.(i) <- if w.e_or_s
      then {we = false ; ws = grid.(i).ws}
      else {we = grid.(i).we ;ws = false};
    let li = 
      if i_not_visited 
      then get_walls_from_cell w
      else [] in
    let lj = 
      if j_not_visited
      then get_walls_from_neighbour w
      else [] in
      List.fold_left add (List.fold_left add wrapper li) lj end

(* val progress : wall array * int -> cell array -> cell array *)
let rec progress (wrapper : wall array * int) grid = 
  if snd wrapper = 0
  then grid
  else  
    let r = Random.int (snd wrapper) in
    let next_w_arr = break grid r wrapper in
    progress next_w_arr grid

let maze =
  Random.self_init ();
  let grid  = Array.make (l * h) {we = true; ws =  true} in
  let init_wrapper = (Array.make (l * h * 4) {x = -1; y = -1; e_or_s = true}, 0) in
  let r_x = Random.int (l) in
  let r_y = Random.int (h) in
  let r_b = Random.bool () in
  let start_arr = add init_wrapper {x = r_x; y = r_y; e_or_s = r_b} in
  display (progress start_arr grid)
