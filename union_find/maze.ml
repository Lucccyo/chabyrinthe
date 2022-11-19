type cell = {
  s: bool;
  e: bool;
  chief: cell;
  rank: int;
}

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp;
  a

let rec shuffle a i =
  if i = 0
  then a
  else (
    let j = Random.int (Array.length a) in
    shuffle (swap a i j) (i-1))

let shuffle a =
  Random.self_init ();
  let l = Array.length a in
  if l = 0 then a
  else shuffle a (Array.length a-1)
