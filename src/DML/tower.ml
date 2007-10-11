qualif LTs(x) : x < Array.length leftPost;;
qualif LEs(x) : x <= Array.length leftPost;;
qualif NEs(x) : not(x = Array.length leftPost);;
qualif SEQs(x) : Array.length x = Array.length leftPost;;
qualif EQs(x) : x = Array.length leftPost;;
qualif LTs1(x) : x < Array.length middlePost;;
qualif LEs1(x) : x <= Array.length middlePost;;
qualif NEs1(x) : not(x = Array.length middlePost);;
qualif EQs1(x) : Array.length x = Array.length middlePost;;
qualif LTs2(x) : x < Array.length rightPost;;
qualif LEs2(x) : x <= Array.length rightPost;;
qualif NEs2(x) : not(x = Array.length rightPost);;
qualif EQs2(x) : Array.length x = Array.length rightPost;;
qualif LTs3(x) : x < Array.length _1_post;;
qualif LEs3(x) : x <= Array.length _1_post;;
qualif NEs3(x) : not(x = Array.length _1_post);;
qualif SEQs3(x) : Array.length x = Array.length _1_post;;
qualif EQs3(x) : x = Array.length _1_post;;
qualif LTs4(x) : x < Array.length post';;
qualif LEs4(x) : x <= Array.length post';;
qualif NEs4(x) : not(x = Array.length post');;
qualif EQs4(x) : Array.length x = Array.length post';;
qualif LTs5(x) : x < Array.length _2_post;;
qualif LEs5(x) : x <= Array.length _2_post;;
qualif NEs5(x) : not(x = Array.length _2_post);;
qualif EQs5(x) : Array.length x = Array.length _2_post;;
qualif LTs6(x) : x < Array.length source;;
qualif LEs6(x) : x <= Array.length source;;
qualif NEs6(x) : not(x = Array.length source);;
qualif EQs6(x) : Array.length x = Array.length source;;

qualif SUM(x) : p + p' + s = Array.length rightpost + Array.length rightpost;;
qualif OSUM(x) : x + n <= Array.length rightpost;;
qualif LTnppn(x) : n <= p';;
qualif P(x) : x = Array.length source + Array.length source - p' - s;;
qualif PP(x) : x = Array.length source + Array.length source - p - s;;
qualif S(x) : x = Array.length source + Array.length source - p' - p;;

qualif LTp(x) : x < p;;

qualif LT0(x) : x < 0;;
qualif LE0(x) : x <= 0;;
qualif NE0(x) : not(x = 0);;
qualif GT0(x) : 0 < x;;
qualif GE0(x) : 0 <= x;;
qualif EQ0(x) : 0 = x;;
qualif BOOL(x) : (x = 0) or (x = 1);;
qualif TRUE(x) : x = 1;;
qualif FALSE(x) : x = 0;;


let print_newline _1_none = () in
let print_string _2_none = () in
let
play sz =
  let leftPost = Array.make sz 0 in
  let middlePost = Array.make sz 0 in
  let rightPost = Array.make sz 0 in
	let sz_minus = sz - 1 in

  let initialize _1_post =
		let _1_i = ref 0 in
		let rec initialize_rec _3_none = 
			let deref_1_i = !_1_i in
			let deref_1_i_plus = deref_1_i + 1 in
			if deref_1_i < sz_minus then (Array.set _1_post deref_1_i deref_1_i_plus; _1_i := deref_1_i_plus; initialize_rec ()) 
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
      (showpiece (Array.get leftPost deref_3_i);
			print_string ();
      showpiece (Array.get middlePost deref_3_i);
			print_string ();
      showpiece (Array.get rightPost deref_3_i);
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
      begin (Array.set _2_post (p - 1) (Array.get source s); Array.set source s 0; showposts()) end
    else begin
      (move _2_n_minus source s post' p' _2_post p;
      Array.set _2_post p_minus (Array.get source (s_plus_2_n_minus));
      Array.set source (s_plus_2_n_minus) 0;
      showposts ();
      move _2_n_minus post' pp_minus_2_n_plus _2_post p_minus source s_plus_2_n)
    end
	in
  (showposts ();
  move sz leftPost 0 rightPost sz middlePost sz)
in play 10 
;;


(*
let{Array.length:int | Array.length > 0}
play Array.length =
  let leftPost = Array.make_vect Array.length 0
  and middlePost = Array.make_vect Array.length 0
  and rightPost = Array.make_vect Array.length 0 in

  let initialize post =
    for i = 0 to Array.length - 1 do
      post..(i) <- i+1
    done
  withtype int vect(Array.length) -> unit in

  let showpiece n =
    for i = 1 to n do print_string "O" done;
    for i = n + 1 to Array.length do print_string " " done in

  let showposts () =
    for i = 0 to Array.length - 1 do
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
            p <= Array.length /\ p' <= Array.length /\ s + p + p' = Array.length + Array.length /\ 0 < n /\
            s + n <= Array.length /\ n <= p /\ n <= p' }
           int(n) * int vect(Array.length) * int(s) * int vect(Array.length) * int(p) * int vect(Array.length) * int(p') -> unit in
  showposts();
  move(Array.length, leftPost, 0, rightPost, Array.length, middlePost, Array.length)
withtype int(Array.length) -> unit
;; *)
