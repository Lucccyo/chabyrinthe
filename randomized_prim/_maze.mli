type cell = {we: bool; ws: bool}
type wall = {x:int; y: int; e_or_s: bool}

val display : cell array -> unit
val maze : unit -> unit