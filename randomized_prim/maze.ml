type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}

(* val add : wall array * int -> wall -> wall array * int *)
let add (w_arr : wall array * int) wall =
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  w_arr.(l - 1) <- wall;
  (w_arr, l + 1)
  
(* val remove : wall array * int -> int -> wall array * int  *)
let remove (w_arr : wall array * int) r = 
  let l     = snd w_arr in
  let w_arr = fst w_arr in
  w_arr.(r) <- w_arr.(l - 1);
  (w_arr, l - 1)  

val is_not_visited grid c i = 
  let n = i - l in
  let w = i - 1 in
  let wn = try (grid.(n)).ws with true in
  let ww = try (grid.(w)).we with true in
  c.we && c.ws && wn && ww

(* val break : cell array -> int -> int -> bool -> int *)
let break grid x y e l =
  let i = y * l + x in
  let c_i = grid.(i) in
  let neighbour = if e then i + 1 else i + l in
  let c_neighbour = grid.(neighbour) in
  is_not_visited grid c_i i lor is_not_visited grid c_neighbour neighbour


  

