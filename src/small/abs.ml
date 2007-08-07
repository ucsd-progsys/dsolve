qualifier NNEG(x) = 0 <= x;;

let abs n = if n < 0 then 0 - n else n in
  abs (-3);;
