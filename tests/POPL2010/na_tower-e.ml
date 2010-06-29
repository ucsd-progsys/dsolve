(* DSOLVE -dontminemlq *)

let print_newline _ = () 
let print_string _ = () 

let play sz =
  let leftPost = Junkarray2.make sz 0 in
  let middlePost = Junkarray2.make sz 0 in
  let rightPost = Junkarray2.make sz 0 in
  let sz_minus = sz - 1 in

  let initialize post =
    for i = 0 to sz - 1 do
        Junkarray2.set post i (i+1)
    done;
  in

  let showpiece n =
    for i = 1 to n do print_string "O" done;
    for i = n + 1 to sz do print_string " " done
  in 

  let showposts _ =
    for i = 0 to sz - 1 do 
      showpiece (Junkarray2.get leftPost i);
      print_string ();
      showpiece (Junkarray2.get middlePost i);
      print_string ();
      showpiece (Junkarray2.get rightPost i);
      print_newline ();
    done;
  in

  let _ = initialize leftPost in
  
  let rec move n source s post p post' p' =
    if n = 1 then begin
      Junkarray2.set post (p-1) (Junkarray2.get source s); 
      Junkarray2.set source s 0; 
      showposts () 
    end else begin
      move (n-1) source s post' p' post p;
      Junkarray2.set post (p-1) (Junkarray2.get source (s + n (* - 1 *))); 
      Junkarray2.set source (s + n-1) 0;
      (showposts ());
      move (n-1) post' (p' - (n-1)) post (p-1) source (s+n)
    end
  in
  showposts ();
  move sz leftPost 0 rightPost sz middlePost sz

let driver =
  let n = read_int () in
  if n > 0 then play n else ()
