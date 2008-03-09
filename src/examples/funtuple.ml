squalif TWO_BETTER_THAN_ONE(t): t.0 < t.1

let produce _ =
  (5, 20)

let _ = match produce () with (a, b) -> assert(a < b)
