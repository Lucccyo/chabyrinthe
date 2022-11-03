type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}

let l = 4
let h = 3

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

(* val is_not_visited : cell array -> cell -> int -> bool *)
let is_not_visited grid c i = 
  let n = i - l in
  let w = i - 1 in
  let wn = try (grid.(n)).ws with Invalid_argument _e -> true in
  let ww = try (grid.(w)).we with Invalid_argument _e -> true in
  c.we && c.ws && wn && ww

let break grid x y e r w_arr=
  let i = y * l + x in
  let c_i = grid.(i) in
  let j = if e then i + 1 else i + l in
  let c_j = grid.(j) in
  let breakable = is_not_visited grid c_i i lor is_not_visited grid c_j j in
  if breakable then 
    match e with 
    | true  -> 
      add w_arr {x = x + 1; y; e_or_s = true}
      add w_arr {x = x + 1; y; e_or_s = false}
      add w_arr {x = x + 1; y = y - 1; e_or_s = false}
    | false -> 
      add w_arr {x; y = y + 1; e_or_s = true}
      add w_arr {x; y = y + 1; e_or_s = false}
      add w_arr {x = x - 1; y = y + 1; e_or_s = true};
  remove w_arr r
  (* then (
    
  )  *)




