let rec sum x = if x <= 0 then 0 else x + sum (x - 1)

let m = read_int ()
let _ = assert (m <= sum m)

let main m = assert (m <= sum m)

