qual LEX(z): z <= x;;
qual LEY(z): z <= y;;
qual LU(z): z < u;;
qual LV(z): z < v;;
qual GZ(z): 0 <= z;;

? 
let min = fun x -> fun y -> if x < y then x else y in
let dot = fun u -> fun v ->
  let a = u - 1 in
  let b = v - 1 in
  let n = min a b in
  letrec mult = fun i -> fun s ->
    if n < i then
      s
    else
      let s = (fun k -> k) i in
	mult (i + 1) (s + i)
  in
    mult 0 0
in
let w = dot 1 2 in
  dot 3 4;;
