squalif TWO_BETTER_THAN_ONE(t): t.1 < t.2

let test pr =
  match pr with
    | (a, b) ->
        assert(a < b)

let x = 2
let y = 1
let p = (x, y)
let _ = test p
