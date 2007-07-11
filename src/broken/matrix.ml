pred L(x): 0 <= x;;
pred AR(x): x < arows;;
pred AC(x): x < acols;;
pred BR(x): x < brows;;
pred BC(x): x < bcols;;
pred N(x): x < 0;;

? let matmult = fun a -> fun ar -> fun ac -> fun b -> fun br -> fun bc -> fun d ->
  if ar = bc then
    if ac = br then
      let dot = fun r -> fun c -> fun i -> fun s ->
	if ac < i then
	  s
	else
	  let t = s + (Ref(a, r, i) + Ref(b, i, c)) in
	    dot r c (i + 1) t
      in
      let mul = fun y -> fun x ->
	if ar < y then
	  d
	else
	  if bc < x then
	    mul (y + 1) 0
	  else
	    let z = dot y x 0 0 in
	    let q = Set(d, y, x, z) in
	      mul y (x + 1)
      in
	mul 0 0
    else
      d
  else
    d
in
  matmult am (arows - 1) (acols - 1) bm (brows - 1) (bcols - 1) dm;;
