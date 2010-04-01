let rec mult x y = if (x <= 0 || y <= 0) then 0 else x + (mult x (y - 1))
and main () = assert (600 <= mult 100 5)
