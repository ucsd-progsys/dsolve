qualifier LTs(x) = x < size leftPost;;
qualifier LEs(x) = x <= size leftPost;;
qualifier NEs(x) = not(x = size leftPost);;
qualifier SEQs(x) = size x = size leftPost;;
qualifier EQs(x) = x = size leftPost;;
qualifier LTs1(x) = x < size middlePost;;
qualifier LEs1(x) = x <= size middlePost;;
qualifier NEs1(x) = not(x = size middlePost);;
qualifier EQs1(x) = size x = size middlePost;;
qualifier LTs2(x) = x < size rightPost;;
qualifier LEs2(x) = x <= size rightPost;;
qualifier NEs2(x) = not(x = size rightPost);;
qualifier EQs2(x) = size x = size rightPost;;
qualifier LTs3(x) = x < size _1_post;;
qualifier LEs3(x) = x <= size _1_post;;
qualifier NEs3(x) = not(x = size _1_post);;
qualifier SEQs3(x) = size x = size _1_post;;
qualifier EQs3(x) = x = size _1_post;;
qualifier LTs4(x) = x < size post';;
qualifier LEs4(x) = x <= size post';;
qualifier NEs4(x) = not(x = size post');;
qualifier EQs4(x) = size x = size post';;
qualifier LTs5(x) = x < size _2_post;;
qualifier LEs5(x) = x <= size _2_post;;
qualifier NEs5(x) = not(x = size _2_post);;
qualifier EQs5(x) = size x = size _2_post;;
qualifier LTs6(x) = x < size source;;
qualifier LEs6(x) = x <= size source;;
qualifier NEs6(x) = not(x = size source);;
qualifier EQs6(x) = size x = size source;;

qualifier SUM(x) = p + p' + s = size rightpost + size rightpost;;
qualifier OSUM(x) = x + n <= size rightpost;;
qualifier LTnppn(x) = n <= p';;
qualifier P(x) = x = size source + size source - p' - s;;
qualifier PP(x) = x = size source + size source - p - s;;
qualifier S(x) = x = size source + size source - p' - p;;

qualifier LTp(x) = x < p;;

qualifier LT0(x) = x < 0;;
qualifier LE0(x) = x <= 0;;
qualifier NE0(x) = not(x = 0);;
qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier EQ0(x) = 0 = x;;
qualifier BOOL(x) = (x = 0) or (x = 1);;
qualifier TRUE(x) = x = 1;;
qualifier FALSE(x) = x = 0;;


let print_newline _1_none = () in
let print_string _2_none = () in
let
play sz =
  let leftPost = make sz 0 in
  let middlePost = make sz 0 in
  let rightPost = make sz 0 in
	let sz_minus = sz - 1 in

  let initialize _1_post =
		let _1_i = ref 0 in
		let rec initialize_rec _3_none = 
			let deref_1_i = !_1_i in
			let deref_1_i_plus = deref_1_i + 1 in
			if deref_1_i < sz_minus then (set _1_post deref_1_i deref_1_i_plus; _1_i := deref_1_i_plus; initialize_rec ()) 
											 else ()
		in initialize_rec ()
  in

  let showpiece _1_n =
		let _1_n_plus = _1_n + 1 in
		let _2_i = ref 1 in
		let rec showpiece_r_rec _4_none =
			let deref_2_i = !_2_i in
			let deref_2_i_plus = deref_2_i + 1 in
			if deref_2_i_plus < _1_n then (print_string (); _2_i := deref_2_i_plus; showpiece_r_rec ())
										 else ()
		in
		let j = ref _1_n_plus in
		let rec showpiece_r2_rec _5_none =		
			let deref_j = !j in
			let deref_j_plus = deref_j + 1 in
			if deref_j < sz then (print_string (); j := deref_j_plus; showpiece_r2_rec ())
										 else ()
		in (showpiece_r_rec (); showpiece_r2_rec ())
	in

  let showposts _6_none =
		let _3_i = ref 0 in
		let rec showposts_rec _6_none =
			let deref_3_i = !_3_i in
			let deref_3_i_plus = deref_3_i + 1 in
			if deref_3_i < sz_minus then
      (showpiece (get leftPost deref_3_i);
			print_string ();
      showpiece (get middlePost deref_3_i);
			print_string ();
      showpiece (get rightPost deref_3_i);
      print_newline ();
			_3_i := deref_3_i_plus;
			showposts_rec ())
			else ()
		in (showposts_rec (); print_newline ())
  in

  let _7_none = initialize leftPost in
  let rec move _2_n source s _2_post p post' p' =
		let _2_n_minus = _2_n - 1 in
		let _2_n_plus = _2_n + 1 in
		let p_minus = p - 1 in
		let s_plus_2_n_minus = s + _2_n_minus in
		let pp_minus_2_n_plus = p' - _2_n_minus in
		let s_plus_2_n = s + _2_n in
    if _2_n = 1 then
      begin (set _2_post (p - 1) (get source s); set source s 0; showposts()) end
    else begin
      (move _2_n_minus source s post' p' _2_post p;
      set _2_post p_minus (get source (s_plus_2_n_minus));
      set source (s_plus_2_n_minus) 0;
      showposts ();
      move _2_n_minus post' pp_minus_2_n_plus _2_post p_minus source s_plus_2_n)
    end
	in
  (showposts ();
  move sz leftPost 0 rightPost sz middlePost sz)
in play 10 
;;


(*
let{size:int | size > 0}
play size =
  let leftPost = make_vect size 0
  and middlePost = make_vect size 0
  and rightPost = make_vect size 0 in

  let initialize post =
    for i = 0 to size - 1 do
      post..(i) <- i+1
    done
  withtype int vect(size) -> unit in

  let showpiece n =
    for i = 1 to n do print_string "O" done;
    for i = n + 1 to size do print_string " " done in

  let showposts () =
    for i = 0 to size - 1 do
      showpiece leftPost..(i);
      print_string "  ";
      showpiece middlePost..(i);
      print_string "  ";
      showpiece rightPost..(i);
      print_newline ()
    done;
    print_newline ()
  withtype unit -> unit in

  let _ = initialize(leftPost) in
  let rec move (n, source, s, post, p, post', p') =
    if eq_int n 1 then
      begin post..(p - 1) <- source..(s); source..(s) <- 0; showposts() end
    else begin
      move(n-1, source, s, post', p', post, p);
      post..(p - 1) <- source..(s + n - 1);
      source..(s + n - 1) <- 0;
      showposts ();
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
  withtype {n:nat}{s:nat}{p:nat}{p':nat |
            p <= size /\ p' <= size /\ s + p + p' = size + size /\ 0 < n /\
            s + n <= size /\ n <= p /\ n <= p' }
           int(n) * int vect(size) * int(s) * int vect(size) * int(p) * int vect(size) * int(p') -> unit in
  showposts();
  move(size, leftPost, 0, rightPost, size, middlePost, size)
withtype int(size) -> unit
;; *)
