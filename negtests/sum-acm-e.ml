let rec sum x y k = if x <= 0 then k y else sum (x - 1) (x + y) k
and check x = assert (100 <= x);check x
let main () = sum 5 0 check
