let rec mult x y = if (x <= 0 || y <= 0) then 0 else x + (mult x (y - 1))
and h y = assert (y <= mult y y);h (y + 1)
and main () = h 0
