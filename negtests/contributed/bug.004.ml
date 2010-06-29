(* DSOLVE -no-anormal *)

(* If r_2 is a function, this works ok.
 * If r_2 is a value, f gets a bogus type.
 *)
let rec (f, r_2) = (fun (x: int) -> x), 0

let _ = assert (f 3 = 10000)

