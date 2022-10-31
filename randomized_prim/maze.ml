type cell = {
  x: int;
  y: int;
  mutable east:  bool;
  mutable south: bool;
}

(* let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2) *)


let l = 3
let h = 4

let grid = 
  List.init (l*h) (fun i ->
    {x = i mod l; y = i  /  l; east = false; south = false})

let swap l i r =
  let temp_i = List.nth l i in
  let temp_r = List.nth l r in
  let l = List.mapi (fun index elem -> if index = i then temp_r else elem) l in
  List.mapi (fun index elem -> if index = r then temp_i else elem) l

let rec shuffle n l =
  if n = 0 
  then l
  else (
    Random.self_init ();
    let r = Random.int (List.length l) in
    let l = swap l n r in
    shuffle (n-1) l
  )

let shuffle l = shuffle ((List.length l) - 1) l
