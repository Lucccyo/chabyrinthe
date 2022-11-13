open Printf

type wall = {x: int; y:int; e_or_s: bool}

let l = 4

let h = 4

let i x y = y * l + x

(* true  => there is a wall  *)
(* false => there is no wall *)
let hor = Array.make (l * (h-1)) true (* _ *)

let ver = Array.make ((l-1 * h)) true (* | *)

let display () = 
  for i = 0 to Array.length hor do
    if hor.(i) then printf "__" else printf "  "
  done;
  for i = 0 to Array.length ver do
    if ver.(i) then printf "|" else printf " "
  done;
  printf "\n"

let add (arr, l) w = 
  arr.(!l) <- w;
  l := !l + 1
  (* arr, l + 1 *)

let remove (arr, l) r =
  arr.(r) <- arr.(!l - 1);
  l := !l - 1
  (* arr, l - 1 *)
  
let break dyn r = 
  let w = (fst dyn).(r) in
  remove dyn r;
  if w.e_or_s
  then
    let cond_a = ver.(i (w.x-1) w.y) && hor.(i w.x (w.y-1)) && hor.(i w.x w.y) in
    let cond_b = ver.(i (w.x+1) w.y) && hor.(i (w.x+1) (w.y-1)) && hor.(i (w.x+1) w.y) in
    if cond_a (* west *)
    then (
      if w.x > 0     then add dyn {x = w.x-1; y = w.y; e_or_s = true};
      if w.y > 0     then add dyn {x = w.x; y = w.y-1; e_or_s = false};
      if w.y < (h-1) then add dyn {x = w.x; y = w.y; e_or_s = false});
    if cond_b (* east *)
    then (
      if w.x < (l-1) then add dyn {x = w.x+1; y = w.y; e_or_s = true};
      if w.y > 0     then add dyn {x = w.x+1; y = w.y-1; e_or_s = false};
      if w.y < (h-1) then add dyn {x = w.x+1; y = w.y; e_or_s = false});
    if cond_a || cond_b then ver.(i w.x w.y) <- false
  else
    let cond_a = hor.(i w.x (w.y-1)) && ver.(i (w.x-1) w.y) && ver.(i w.x w.y) in
    let cond_b = hor.(i w.x (w.y+1)) && ver.(i (w.x-1) (w.y+1)) && ver.(i w.x (w.y+1)) in
    if cond_a (* north *)
    then (
      if w.y > 0     then add dyn {x = w.x; y = w.y-1; e_or_s = false};
      if w.x > 0     then add dyn {x = w.x-1; y = w.y; e_or_s = true};
      if w.x < (l-1) then add dyn {x = w.x; y = w.y; e_or_s = true});
    if cond_b (* south *)
    then (
      if w.y < (h-1) then add dyn {x = w.x; y = w.y+1; e_or_s = false};
      if w.x > 0     then add dyn {x = w.x-1; y = w.y+1; e_or_s = true};
      if w.x < (l-1) then add dyn {x = w.x; y = w.y+1; e_or_s = true});
    if cond_a || cond_b then hor.(i w.x w.y) <- false

let rec progress dyn =
  if snd dyn = ref 0 then ()
  else (
    let r = Random.int !(snd dyn) in
    break dyn r;
    progress dyn)

let maze = 
  let dyn = (Array.make (l * h * 2 - l - h) {x = -1; y = -1; e_or_s = true}, ref 0) in
  add dyn {x = Random.int (l); y = Random.int (h); e_or_s = Random.bool ()};
  progress dyn 