let assume_zero x =
  assume (x = 0)

let check (f: int -> unit) x =
  f x

let test x =
  check assume_zero x;
  assert (x = 0)

