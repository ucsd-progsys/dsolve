let rec sum x = if x <= 0 then 0 else x + sum (x - 1)
let rec h y = assert ((y + y) <= sum y);h (y + 1)
let main () = h 0
