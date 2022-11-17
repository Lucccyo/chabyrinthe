open Printf

type wall = {x: int; y:int; e_or_s: bool}

let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)

let hor_i x y = y * l + x
let ver_i x y = y * (l - 1) + x

let hor = Array.make (l * (h-1)) true (* _ *)

let ver = Array.make ((l-1) * h) true (* | *)

let add (arr, l) w =
  arr.(!l) <- w;
  l := !l + 1
  (* arr, l + 1 *)

let remove (arr, l) r =
  arr.(r) <- arr.(!l - 1);
  l := !l - 1
  (* arr, l - 1 *)

let b_hor x y =
  if 0 <= x && x < l && 0 <= y && y < h-1 then hor.(hor_i x y) else true

let b_ver x y =
  if 0 <= x && x < l-1 && 0 <= y && y < h then ver.(ver_i x y) else true

let display () =
  for i = 0 to l - 1 do printf "___" done;
  printf "_";
  for y = 0 to h-1 do
    printf "\n|";
    for x = 0 to l-1 do
      if b_hor x y then printf "__" else printf "  ";
      if b_ver x y
      then printf "|"
      else
        if b_hor x y || b_hor (x + 1) y
        then printf "_"
        else printf " "
    done
  done;
  printf "\n%!"

let break dyn r =
  let w = (fst dyn).(r) in
  remove dyn r;
  if w.e_or_s
  then (
    let i_not_visited = (b_ver (w.x-1) w.y) && (b_hor w.x (w.y-1)) && (b_hor w.x w.y) in
    let j_not_visited = (b_ver (w.x+1) w.y) && (b_hor (w.x+1) (w.y-1)) && (b_hor (w.x+1) w.y) in
    if i_not_visited (* west *)
    then (
      if w.x > 0     then add dyn {x = w.x-1; y = w.y; e_or_s = true};
      if w.y > 0     then add dyn {x = w.x; y = w.y-1; e_or_s = false};
      if w.y < (h-1) then add dyn {x = w.x; y = w.y; e_or_s = false});
    if j_not_visited (* east *)
    then (
      if w.x < (l-2) then add dyn {x = w.x+1; y = w.y; e_or_s = true};
      if w.y > 0     then add dyn {x = w.x+1; y = w.y-1; e_or_s = false};
      if w.y < (h-1) then add dyn {x = w.x+1; y = w.y; e_or_s = false});
    if i_not_visited || j_not_visited then ver.(ver_i w.x w.y) <- false)
  else (
    let i_not_visited = (b_hor w.x (w.y-1)) && (b_ver (w.x-1) w.y) && (b_ver w.x w.y) in
    let j_not_visited = (b_hor w.x (w.y+1)) && (b_ver (w.x-1) (w.y+1)) && (b_ver w.x (w.y+1)) in
    if i_not_visited (* north *)
    then (
      if w.y > 0     then add dyn {x = w.x; y = w.y-1; e_or_s = false};
      if w.x > 0     then add dyn {x = w.x-1; y = w.y; e_or_s = true};
      if w.x < (l-1) then add dyn {x = w.x; y = w.y; e_or_s = true});
    if j_not_visited (* south *)
    then (
      if w.y < (h-2) then add dyn {x = w.x; y = w.y+1; e_or_s = false};
      if w.x > 0     then add dyn {x = w.x-1; y = w.y+1; e_or_s = true};
      if w.x < (l-1) then add dyn {x = w.x; y = w.y+1; e_or_s = true});
    if i_not_visited || j_not_visited then hor.(hor_i w.x w.y) <- false)

let rec progress dyn =
  if snd dyn = ref 0 then ()
  else (
    let r = Random.int !(snd dyn) in
    break dyn r;
    progress dyn)

let maze =
  Random.self_init ();
  let dyn = (Array.make (l * h * 2 - l - h) {x = -1; y = -1; e_or_s = true}, ref 0) in
  add dyn {x = Random.int (l); y = Random.int (h); e_or_s = Random.bool ()};
  progress dyn;
  display ()
