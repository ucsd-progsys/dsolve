let print_newline _ = () 
let print_string _ = () 

let play sz =
  let leftPost = Array.make sz 0 in
  let middlePost = Array.make sz 0 in
  let rightPost = Array.make sz 0 in
	let sz_minus = sz - 1 in

  let initialize post =
		let rec init_rec i =
			if i < sz_minus then
        let _ = Array.set post i (i+1) in
        init_rec (i+1)
		  else ()
		in init_rec 0
  in

  let showpiece n =
		let rec r_rec i =
      if i+1 < n then (print_string (); r_rec (i+1))
										  else ()
		in
		let rec r2_rec j =		
			if j < sz then (print_string (); r2_rec (j+1))
										 else ()
		in (r_rec 1; r2_rec (n+1))
	in

  let showposts _ =
		let rec show_rec i =
			if i < sz_minus then
      (showpiece (Array.get leftPost i);
			print_string ();
      showpiece (Array.get middlePost i);
			print_string ();
      showpiece (Array.get rightPost i);
      print_newline ();
			show_rec (i+1))
			else ()
		in (show_rec 0; print_newline ())
  in

  let _ = initialize leftPost in
  let rec move n source s post p post' p' =
    if n = 1 then
      let gss = Array.get source s in
      begin Array.set post (p-1) gss; Array.set source s 0; (showposts ()) end
    else begin
      (move (n-1) source s post' p' post p;
      let gs = Array.get source (s + n-1) in
      Array.set post (p-1) gs;
      Array.set source (s + n-1) 0;
      (showposts ());
      move (n-1) post' (p' - (n-1)) post (p-1) source (s+n))
    end
	in
  (showposts ();
  move sz leftPost 0 rightPost sz middlePost sz)

let driver x =
  if x > 0 then play x else ()

(*
  let rec move (n, source, s, post, p, post', p') =
    if n = 1 then
      begin post[p - 1] <-  source[s]; source[s] <- 0 end
    else begin
      move(n-1, source, s, post', p', post, p);
      post[p - 1] <- source[s + n - 1];
      source[s + n - 1] <- 0;
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
  move(size, leftPost, 0, rightPost, size, middlePost, size)
*)


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
    if n = 1 then
      begin post[p - 1] <-  source[s]; source[s] <- 0 end
    else begin
      move(n-1, source, s, post', p', post, p);
      post[p - 1] <- source[s + n - 1];
      source[s + n - 1] <- 0;
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
  move(size, leftPost, 0, rightPost, size, middlePost, size)
;; *)
