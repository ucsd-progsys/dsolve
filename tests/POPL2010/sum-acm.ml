let rec sum x y k = if x <= 0 then k y else sum (x - 1) (x + y) k
let rec check x = assert (100 <= x);check x
let _ = sum 100 0 check
