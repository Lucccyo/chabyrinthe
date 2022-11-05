type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}
type dyn_w = (wall array * int)

let l = 4
let h = 4

(* val add : dyn_w -> wall -> dyn_w *)
let add (w_arr : wall array * int) wall =
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  w_arr.(l) <- wall;
  (w_arr, l + 1)
  
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

(* val break : wall -> cell array -> int -> dyn_w -> dyn_w *)
let break w grid r w_arr =
  let i = w.y * l + w.x in
  Format.printf "x = %d; y = %d; l = %d; i = %d\n"w.x w.y l i;
  let c_i = grid.(i) in
  Format.printf "bou2\n";
  let i_not_visited = is_not_visited grid c_i i in
  let j = if w.e_or_s then i + 1 else i + l in
  let j_not_visited =
    (try 
      is_not_visited grid (grid.(j)) j 
    with Invalid_argument _ -> false) in
  let breakable = i_not_visited || j_not_visited in
  if not breakable
  then remove w_arr r
  else (
    let li = 
      if not i_not_visited 
      then []
      else 
        match w.e_or_s with 
        | true  -> 
          [{x = w.x; y = w.y; e_or_s = false}; 
          {x = w.x - 1; y = w.y; e_or_s = true}; 
          {x = w.x; y = w.y - 1; e_or_s = false}]
        | false -> 
          [{x = w.x; y = w.y; e_or_s = true};
          {x = w.x - 1; y = w.y; e_or_s = true}; 
          {x = w.x; y = w.y - 1; e_or_s = false}] in
    let lj = 
      if not j_not_visited
      then []
      else
        match w.e_or_s with 
        | true  -> 
          [{x = w.x + 1; y = w.y + 1; e_or_s = true}; 
          {x = w.x + 1; y = w.y + 1; e_or_s = false}; 
          {x = w.x + 1; y = w.y - 1; e_or_s = false}]
        | false -> 
          [{x = w.x + 1; y = w.y + 1; e_or_s = true};
          {x = w.x + 1; y = w.y + 1; e_or_s = false};
          {x = w.x - 1; y = w.y + 1; e_or_s = true}] in
      let l = li @ lj in
      let updated = List.fold_left (fun acc e -> add acc e) w_arr l in
      remove updated r
      )

(* val progress : dyn_w -> cell array -> cell array *)
let rec progress (w_arr : wall array * int) grid = 
  if snd w_arr = 0
  then grid
  else  
    let r = Random.int (snd w_arr) in
    let next_w_arr = break (fst w_arr).(r) grid r w_arr in
    progress next_w_arr grid 

let rec init_walls i walls col line = 
  Format.printf "i == %d\n" i;
  if i = Array.length walls
  then walls
  else (
    Format.printf "x = %d\ty = %d\teast = %b\n" (col) (line) (true);
    walls.(i)   <- {x = i mod l; y = i / l; e_or_s = true};
    Format.printf "x = %d\ty = %d\teast = %b\n" (col) (line) (false);
    walls.(i+1) <- {x = i mod l; y = i / l; e_or_s = false};
    let new_col  = (col+1) mod l in
    let new_line = (if col = new_col - 1 then line else line + 1) in
    init_walls (i+2) walls new_col new_line)

let init l h =
  let grid  = Array.make (l * h) {we = true; ws =  true} in
  let walls = Array.make (l * h * 2) {x = 0; y = 0; e_or_s = true} in
  let walls = init_walls 0 walls 0 0 in
  (* for i = 0 to ((l * h * 2) - 1) do
    for x = 0 to (l-1) do
      for y = 0 to (h-1) do

      done
    done
    Format.printf "x = %d\ty = %d\teast = %b\n" (i mod l) (i / l) (i mod 2 = 0);
    walls.(i) <- {x = (i mod l); y = (i / l); e_or_s = i mod 2 = 0}
  done; *)
  let init_arr = (Array.make (l * h * 2) {x = 0; y = 0; e_or_s = true}, 0) in
  let r = Random.int (l * h * 2) in
  (* Format.printf "r = %d\t\tinit_arr = %d\t\tl_init_arr = %d\t\twalls = %d\n%!" r (Array.length (fst init_arr)) (snd init_arr) (Array.length walls); *)
  let start_arr = add init_arr walls.(r) in
  (* let grid = progress w_arr grid in *)
  ignore(progress start_arr grid)




