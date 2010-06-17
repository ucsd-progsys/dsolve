let rec mult x y k = if (x <= 0 || y <= 0) then k 0 else mult x (y - 1) (acc x k)
and acc z m r = m (z + r)
and check100 w = assert (600 <= w)
let main () = mult 100 5 check100
