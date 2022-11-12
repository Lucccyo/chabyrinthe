type wall = {x: int; y:int; here: bool}

let l = 4

let h = 4

let hor = Array.make (l * (h-1)) true (* _ *)

let ver = Array.make ((l-1 * h)) true (* | *)

let break w is_hor =
  if is_hor
  then
    if not_visited hor(x, y-1) ver(x-1,y) ver(x,y) (* north *)
    then (
      if w.y > 0     then add hor(x, y-1)
      if w.x > 0     then add ver(x-1,y)
      if w.x < (l-1) then add ver(x,y))
    if not_visited hor(x, y+1) ver(x-1, y+1) ver(x, y+1) (* south *)
    then (
      if w.y < (h-1) then add hor(x, y+1)
      if w.x > 0     then add ver(x-1, y+1)
      if w.x < (l-1) then add ver(x, y+1))
  else
    if not_visited ver(x-1, y) hor(x, y-1) hor(x, y) (* west *)
    then (
      if w.x > 0     then add ver(x-1, y)
      if w.y > 0     then add hor(x, y-1)
      if w.y < (h-1) then add hor(x, y))
    if not_visited ver(x+1, y) hor(x+1, y-1) hor(x+1, y) (* east *)
    then (
      if w.x < (l-1) then add ver(x+1, y)
      if w.y > 0     then add hor(x+1, y-1)
      if w.y < (h-1) then add hor(x+1, y))

