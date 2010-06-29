(* DSOLVE -no-anormal *)

(* If r_2 is a function, this works ok.
 * If r_2 is a value, f gets a bogus type.
 *)
let rec f (x: int) = x
and r_2 = 0

let _ = assert (f 3 = 10000)

