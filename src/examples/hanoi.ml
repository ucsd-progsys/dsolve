pred L(x): 0 <= x;;
pred U(x): x < theight;;
pred T(x): x = theight;;

? let hanoi = fun h ->
  let move = fun n -> fun l -> fun lh -> fun m -> fun mh -> fun r -> fun rh ->
    if n = 1 then
      let a = Unset(l, lh) in
      let b = Set(r, rh) in
	true
    else
      let c = move 1 l lh r rh m mh in
      let d = move (n - 1) l (lh - 1) m (mh + 1) r rh in
      let e = move 1 m (mh + 1) l (lh - 1) r rh in
	true
  in
    move h lt (h - 1) mt 0 rt 0
in
  hanoi theight;;
