pred U(x): x < rlen;;
pred L(x): 0 <= x;;

? let bubsort = fun a -> fun l ->
  let sorted = fun i ->
    if i <= l then
      if Ref(a, i) <= Ref(a, (i + 1)) then
	sorted (i + 1)
      else
	false
    else
      true
  in
  let bubble = fun j ->
    if (j + 1) <= l then
      if Ref(a, (j + 1)) < Ref(a, j) then
	let s = Swap(b, j, (j + 1)) in
	  bubble (j + 1)
      else
	bubble (j + 1)
    else
      a
  in
    if sorted 0 then
      a
    else
      let k = bubble 0 in
	bubsort a l
in
  bubsort r (rlen - 1);;
