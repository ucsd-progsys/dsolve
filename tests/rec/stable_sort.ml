(** sorting -- from ocaml's List.ml *)

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if h1 <= h2 (* cmp h1 h2 <= 0 *)
      then h1 :: merge (* cmp *) t1 l2
      else h2 :: merge (* cmp *) l1 t2

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end

let rec rev_merge l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      if h1 <= h2 (* cmp h1 h2 <= 0 *)
      then rev_merge t1 l2 (h1::accu)
      else rev_merge l1 t2 (h2::accu)

let rec rev_merge_rev l1 l2 accu =
  match l1, l2 with
  | [], l2 -> rev_append l2 accu
  | l1, [] -> rev_append l1 accu
  | h1::t1, h2::t2 ->
      if h1 > h2 (* cmp h1 h2 > 0 *)
      then rev_merge_rev t1 l2 (h1::accu)
      else rev_merge_rev l1 t2 (h2::accu)

let stable_sort (* cmp *) l =
      let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if x1 <= x2 (* cmp x1 x2 <= 0 *) then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if x1 <= x2 (* cmp x1 x2 <= 0 *) then begin
         if x2 <= x3 (* cmp x2 x3 <= 0 *) then [x1; x2; x3]
         else if x1 <= x3 (* cmp x1 x3 <= 0 *) then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if x1 <= x3 (* cmp x1 x3 <= 0 *) then [x2; x1; x3]
         else if x2 <= x3 (* cmp x2 x3 <= 0 *) then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if x1 > x2 (* cmp x1 x2 > 0 *) then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if x1 > x2 (* cmp x1 x2 > 0 *) then begin
         if x2 > x3 (* cmp x2 x3 > 0 *) then [x1; x2; x3]
         else if x1 > x3 (* cmp x1 x3 > 0 *) then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if x1 > x3 (* cmp x1 x3 > 0 *) then [x2; x1; x3]
         else if x2 > x3 (* cmp x2 x3 > 0 *) then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 []
  in
  let len = length l in
  if len < 2 then l else sort len l

let sort = stable_sort
let fast_sort = stable_sort
