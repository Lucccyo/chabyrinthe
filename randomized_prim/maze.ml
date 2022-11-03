type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}
type dyn_w = (wall array * int)

let l = 4
let h = 3

(* val add : dyn_w -> wall -> dyn_w *)
let add (w_arr : wall array * int) wall =
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  w_arr.(l - 1) <- wall;
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
  let c_i = grid.(i) in
  let j = if w.e_or_s then i + 1 else i + l in
  let c_j = grid.(j) in
  let breakable = is_not_visited grid c_i i || is_not_visited grid c_j j in
  let w_arr = 
    if not breakable 
    then w_arr 
    else
      let l = 
        match w.e_or_s with 
        | true  -> 
          [{x = w.x + 1; y = w.y; e_or_s = true}; 
          {x = w.x + 1; y = w.y; e_or_s = false}; 
          {x = w.x + 1; y = w.y - 1; e_or_s = false}]
        | false -> 
          [{x = w.x; y = w.y + 1; e_or_s = true};
          {x = w.x; y = w.y + 1; e_or_s = false};
          {x = w.x - 1; y = w.y + 1; e_or_s = true}] in
      List.fold_left (fun acc e -> add acc e) w_arr l in
  remove w_arr r

let init l h =
  let grid  = Array.make (l*h) {we = true; ws =  true} in
  let walls = 
    (let walls = Array.make (l*h*2) 0 in
    for i = 0 to (l * h * 2 - 1) do
      walls.(i) <- {x = i mod l; y = i / l; e_or_s = i mod 2 = 0}
    done) in walls
  let w_arr = (Array.make (l*h) 0, 0 ) in

  let r = Random.int (l*h*2) in
  let w_arr = break walls.(r) grid r w_arr in
  let _, length = w_arr in
  while length > 0 do
    




