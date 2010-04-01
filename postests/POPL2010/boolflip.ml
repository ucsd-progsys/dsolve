let rec f x y = if x then f y x else g x y
and g x y = assert y; f y x
and h x = assert x; h x
and main () = if (0 < 1) then f (0 < 1) (1 < 0) else h (1 < 0)
