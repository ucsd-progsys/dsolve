squalif TWO_BETTER_THAN_ONE(t): t.1 < t.2

let test pr =
  match pr with
    | (a, b) ->
        assert(a < b)

let x = 1
let y = 2
let p = (x, y)
let _ = test p
