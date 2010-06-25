(* DSOLVE -bare -dontminemlq *)

let rec m x =
  if x > 100
  then x - 10
  else m (m (x + 11))

let n = read_int ()

let _ = if n <= 101 then assert (m n = 91) else ()

