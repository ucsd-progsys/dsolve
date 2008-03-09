squalif TWO_BETTER_THAN_ONE(t): t.0 < t.1

let test pr =
  match pr with
    | (a, b) ->
        assert(a < b)

let x = 1
let y = 2
let p = (x, y)
let _ = test p
