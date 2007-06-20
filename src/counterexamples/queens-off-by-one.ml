pred U(x): x < n;;
pred L(x): 0 <= x;;

? let nqueens = fun a -> fun n ->
  let attacked = fun c -> fun r -> fun d ->
    if c < d then
      false
    else
      let m = if 0 <= r - d then Ref(a, c - d, r - d) else false in
      let k = if r + d < n then Ref(a, c - d, r + d) else false in
      let l = Ref(a, c - d, r) in
      let o = attacked c r (d + 1) in
	m
  in
  let fill = fun i -> fun j ->
    if n <= i then
      true
    else
      if n < j then
	false
      else
	if attacked i j 1 then
	  false
	else
	  let s = Set(a, i, j) in
	    if fill (i + 1) 0 then
	      true
	    else
	      let t = Unset(a, i, j) in
		fill i (j + 1)
  in
    fill 0 0
in
  nqueens b 8;;
