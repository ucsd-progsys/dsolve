(* Demonstrates the handling of NULL-terminated arrays.
   We initialize a 0-filled array with nonzero values from indices 0 to n - 1.
   We then infer a predicate on the array that says
     if the value is 0, then the index is in the range [0, n]
   We then jog over the array, starting at index 0, until we find the first 0 value.
   The presence of the inferred predicate suffices to verify that the index
   we use never exceeds n.
   This is a _very_ close analog of idiomatic C NULL-terminated string handling.
*)

(* Create a 0-filled array and initialize elements [0, n) with values 1, ..., n. *)
let rec mk_array n =
  if n <= 0 then
    Deparray.create 0
  else
    let a = mk_array (n - 1) in
      Deparray.set a (n - 1) n

(* Read the array, beginning with element i and continuing until a 0 is found.
   Parameter n is a witness to the array bounds. *)
let rec read_array (n: int) (i: int) (a: (int, int) Deparray.t) =
  (*** !!! Verify that we stay within [0, n]. !!! ***)
  let _ = assert (0 <= i) in
  let _ = assert (i <= n) in
  let v = Deparray.get a i in
    if v = 0 then
      ()
    else
      read_array n (i + 1) a

let test_null_terminated_array () =
  let n = 100 in
  let a = mk_array n in
    read_array n 0 a
