qual U(x): x < a;;
qual L(x): 0 <= x;;

? let bsearch = fun k -> fun a ->
  letrec look = fun l -> fun h ->
    if l <= h then
      let f = h - l in
      let m = l + f in
	if m = k then
	  (fun z -> z) m
	else
	  if m < k then
	    look l (m - 1)
	  else
	    look (m + 1) h
    else
      -1
  in
    look 0 (a - 1)
in
let w = bsearch 0 10 in
let b = bsearch 5 12 in
  w;;
