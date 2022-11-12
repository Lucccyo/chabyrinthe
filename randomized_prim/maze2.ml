type wall = {x: int; y:int; e_or_s: bool}

let l = 4

let h = 4

let i x y = y * l + x

(* true  => there is a wall  *)
(* false => there is no wall *)
let hor = Array.make (l * (h-1)) true (* _ *)

let ver = Array.make ((l-1 * h)) true (* | *)

let add ((wip, l) : wall array * int) w = 
  wip.(l) <- w;
  wip, l + 1

let remove ((wip, l) : wall array * int) r =
  wip.(r) <- wip.(l - 1);
  wip, l - 1

(* let not_visited a b c =
  a.here && b.here && c.here *)
  
let break w =
  if w.e_or_s
  then
    if ver.(i (x-1) y) && hor.(i x (y-1)) && hor.(i x y) (* west *)
    then (
      if w.x > 0     then add ver(x-1, y)
      if w.y > 0     then add hor(x, y-1)
      if w.y < (h-1) then add hor(x, y))
    if ver.(i (x+1) y) && hor.(i (x+1) (y-1)) && hor.(i (x+1) y) (* east *)
    then (
      if w.x < (l-1) then add ver(x+1, y)
      if w.y > 0     then add hor(x+1, y-1)
      if w.y < (h-1) then add hor(x+1, y))
  else
    if hor.(i x (y-1)) && ver.(i (x-1) y) && ver.(i x y) (* north *)
    then (
      if w.y > 0     then add hor(x, y-1)
      if w.x > 0     then add ver(x-1,y)
      if w.x < (l-1) then add ver(x,y))
    if hor.(i x (y+1)) && ver.(i (x-1) (y+1)) && ver.(i x (y+1)) (* south *)
    then (
      if w.y < (h-1) then add hor(x, y+1)
      if w.x > 0     then add ver(x-1, y+1)
      if w.x < (l-1) then add ver(x, y+1))


let maze = 
  let init_arr = (Array.make (l * h * 2 - l - h) {x = -1; y = -1; e_or_s = true}, 0) in
  let arr = add init_wrapper {x = Random.int (l); y = Random.int (h); e_or_s = Random.bool ()} in