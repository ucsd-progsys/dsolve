(** sorting -- from ocaml's List.ml *)

let show x = x

let rec split_aux lst left right = 
  match lst with
  | []           -> (left, right)
  | [x]          -> (x :: left, right)
  | x :: y :: ys -> split_aux ys (x :: left) (y :: right)

let split lst =
  split_aux lst [] []

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'

let rec chop k l l' =
  if k = 0 then (l,l') else begin
    match l with
    | x::t -> chop (k-1) t (x::l')
    | _ -> assert false
  end

(*
let reverse xs = 
  let rec rev ys rs = 
    match ys with 
    | [] -> rs
    | y::ys' -> rev ys' (y::rs) in
  rev xs []
*)

(* dec -> inc *)
let rec rev k ys zs = 
  match ys with
  | []    -> zs
  | y::ys' -> rev y ys' (y::zs) 

let reverse xs =
  match xs with [] -> [] 
  | x::xs' -> rev x (x::xs') [] 

(* inc -> dec -> dec *)
let rec rev_append w l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_append (show a) l (a :: l2)

let rec rev_append_rev w l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_append_rev (show a) l (a :: l2)

(* inc -> inc -> dec *)
(*let rec rev_merge l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      if h1 <= h2 
      then rev_merge t1 l2 (h1::accu)
      else rev_merge l1 t2 (h2::accu)

(* dec -> inc -> inc *)
let rec rev_append_rev l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_append_rev l (a :: l2)

(* dec -> dec -> inc *)
let rec rev_merge_rev w l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append_rev w l2 accu
  | l1, [] -> rev_append_rev w l1 accu
  | h1::t1, h2::t2 ->
      if h1 > h2 
      then rev_merge_rev t1 l2 (h1::accu)
      else rev_merge_rev l1 t2 (h2::accu)
*)

let rev_merge l1 l2 =
  let rec revm w l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append w l2 accu
  | l1, [] -> rev_append w l1 accu
  | h1::t1, h2::t2 ->
      if h1 <= h2 
      then revm h1 t1 (h2::t2) (h1::accu)
      else revm h2 (h1::t1) t2 (h2::accu) in
  match l1, l2 with
  | [],[] -> [] 
  | [], h2::t2 -> revm h2 [] (h2::t2) [] 
  | h1::t1,[] -> revm h1 (h1::t1) [] [] 
  | h1::t1, h2::t2 ->
      if h1 <= h2 then revm h1 (h1::t1) (h2::t2) [] 
      else revm h2 (h1::t1) (h2::t2) [] 

let rev_merge_rev l1 l2 =
  let rec revm w l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append_rev w l2 accu
  | l1, [] -> rev_append_rev w l1 accu
  | h1::t1, h2::t2 ->
      if h1 > h2 
      then revm h1 t1 (h2::t2) (h1::accu)
      else revm h2 (h1::t1) t2 (h2::accu) in
  match l1, l2 with
  | [],[] -> [] 
  | [], h2::t2 -> revm h2 [] (h2::t2) [] 
  | h1::t1,[] -> revm h1 (h1::t1) [] [] 
  | h1::t1, h2::t2 ->
      if h1 > h2 then revm h1 (h1::t1) (h2::t2) [] 
      else revm h2 (h1::t1) (h2::t2) [] 

let rec ssort l =
  match l with
  | [] -> []
  | x1::[] -> [x1]
  | x1 :: x2 :: [] ->
     if x1 <= x2 then [x1; x2] else [x2; x1]
  | x1 :: x2 :: x3 :: [] ->
     if x1 <= x2 then begin
       if x2 <= x3 then [x1; x2; x3]
       else if x1 <= x3 then [x1; x3; x2]
       else [x3; x1; x2]
     end else begin
       if x1 <= x3 then [x2; x1; x3]
       else if x2 <= x3 then [x2; x3; x1]
       else [x3; x2; x1]
     end 
  | x1 :: x2 :: x3 :: _ ->
     let (l1,l2) = split l in
     let s1 = ssort l1 in
     let s2 = ssort l2 in
     let s  = rev_merge s1 s2 in
     reverse s 

let stable_sort l =
  let rec sort n l =
    match l with
    | [] -> []
    | x1 :: [] -> [x1]
    | x1 :: x2 :: [] ->
       if x1 <= x2 then [x1; x2] else [x2; x1]
    | x1 :: x2 :: x3 :: [] ->
       if x1 <= x2 then begin
         if x2 <= x3 then [x1; x2; x3]
         else if x1 <= x3 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if x1 <= x3 then [x2; x1; x3]
         else if x2 <= x3 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | l ->
       let n1 = n asr 1  in
       let n2 = n - n1 in
       let (l1,l2) = chop n1 l [] in 
       let s1 = rev_sort n1 l1 in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 
  and rev_sort n l =
    match l with
    | [] -> []
    | x1 :: [] -> [x1]
    | x1 :: x2 :: [] ->
       if x1 > x2 then [x1; x2] else [x2; x1]
    | x1 :: x2 :: x3 :: [] ->
       if x1 > x2 then begin
         if x2 > x3 then [x1; x2; x3]
         else if x1 > x3 then [x3; x1; x2] (* BUG swap 3,1*)
         else [x3; x1; x2]
       end else begin
         if x1 > x3 then [x2; x1; x3]
         else if x2 > x3  then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let (l1,l2) = chop n1 l [] in 
       let s1 = sort n1 l1  in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 
  in
  let n = len l in
  (* if n < 2 then l else *)
  sort n l

let rec sortcheck l =
  match l with
    | [] -> ()
    | x :: [] -> ()
    | x :: y :: ys ->
        assert (x <= y); sortcheck (y :: ys)

let check xs = 
  let xs' = ssort xs in
  let _ = sortcheck xs' in 
  let xs'' = stable_sort xs in
  let _ = sortcheck xs'' in 
  (* let ys = [2;1;0] (* [4;3;2;1] *) in
  let _ = show ys in
  let ys' = reverse ys in
  let ys'' = rev_append 0 [0;1;2] [] in 
  let zs = [0;1;2] in
  let ys''' = rev_merge zs zs in 
  ys' *)
  ()
