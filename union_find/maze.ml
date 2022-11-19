type cell = {
  mutable s: bool;
  mutable e: bool;
  mutable chief: cell;
  mutable rank: int;
}

let rec shuffle a i =
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
  if l = 0 then a else shuffle a (l-1)

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

