(* DSOLVE -no-anormal -bare -dontminemlq *)

let rec gcd a b =
  if b = 0 then 0 else gcd b 0;;

let x = gcd 1 2 in assert (x = 2);;

