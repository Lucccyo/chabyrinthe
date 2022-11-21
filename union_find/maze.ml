type cell = {
  mutable s: bool;
  mutable e: bool;
  mutable chief: cell;
  mutable rank: int;
}

let l = 4
let h = 4

(* let rec shuffle a i =
  if i = 0
  then a
  else (
    let j = Random.int (Array.length a) in
    let swap a i j =
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
      a in
    shuffle (swap a i j) (i-1))

let shuffle a =
  Random.self_init ();
  let l = Array.length a in
  if l = 0 then a else shuffle a (l-1) *)

let swap a x y rx ry =
  let tmp = a.(x).(y) in
  a.(x).(y) <- a.(rx).(ry);
  a.(rx).(ry) <- tmp

let shuffle a = 
  Random.self_init ();
  for y = 0 to Array.length a - 1 do
    for x = 0 to Array.length a.(0) -1 do
      let rx = Random.int (Array.length a.(0)) in
      let ry = Random.int (Array.length a) in
      swap a x y rx ry
    done
  done;
  a

let rec find c =
  if c.chief <> c
  then (c.chief <- find c.chief; c.chief)
  else c

let union l r =
  let cl = find l in
  let cr = find r in
  if cl.rank <> cr.rank
  then
    let c_min, c_max = if cl.rank < cr.rank then cl,cr else cr, cl in
    c_min.chief <- c_max
  else (cl.chief <- cr;
    cr.rank <- cr.rank+1)

let () = 
  let rec cell = {
    s = true;
    e = true;
    chief = cell;
    rank = 0;
  } in
  let grid = Array.make h (Array.make l cell) in
  Format.printf "ba"
  