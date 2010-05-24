(* DSOLVE -bare *)

(* let init l f = *)
(*   if l = 0 then [||] else *)
(*    let res = Array.create l (f 0) in *)
(*    for i = 1 to (pred l)  do *)
(*      Array.unsafe_set res i (f i) *)
(*    done; *)
(*    res *)

(* let make_matrix sx sy init = *)
(*   let res = (Array.create sx (Array.create sy init)) (\* [||] *\)  in *)
(*   for x = 0 to pred sx do *)
(*     Array.unsafe_set res x (Array.create sy init) *)
(*   done; *)
(*   res *)

(* let create_matrix = make_matrix *)

(* let copy a = *)
(*   let l = Array.length a in *)
(*   if l = 0 then [||] else begin *)
(*     let res = Array.create l (Array.unsafe_get a 0) in *)
(*     for i = 1 to pred l do *)
(*       Array.unsafe_set res i (Array.unsafe_get a i) *)
(*     done; *)
(*     res *)
(*   end *)

(* let append a1 a2 = *)
(*   let l1 = Array.length a1 and l2 = Array.length a2 in *)
(*   if l1 = 0 && l2 = 0 then [||] else begin *)
(*     let r = Array.create (l1 + l2) (Array.unsafe_get (if l1 > 0 then a1 else a2) 0) in *)
(*     for i = 0 to l1 - 1 do Array.unsafe_set r i (Array.unsafe_get a1 i) done; *)
(*     for i = 0 to l2 - 1 do Array.unsafe_set r (i + l1) (Array.unsafe_get a2 i) done; *)
(*     r *)
(*   end *)

(* let concat_aux init al = *)
(*   let rec size accu = function *)
(*     | [] -> accu *)
(*     | h::t -> size (accu + Array.length h) t *)
(*   in *)
(*   let res = Array.create (size 0 al) init in *)
(*   let rec fill pos = function *)
(*     | [] -> () *)
(*     | h::t -> *)
(*         if (pos + Array.length h <= Array.length res) then begin  *)
(*         for i = 0 to Array.length h - 1 do *)
(*           Array.unsafe_set res (pos + i) (Array.unsafe_get h i); *)
(*         done; *)
(*         fill (pos + Array.length h) t; *)
(*         end *)
(*   in *)
(*   fill 0 al; *)
(*   res *)
(* ;; *)

(* let concat al = *)
(*   let rec find_init = function *)
(*       [] -> [||] *)
(*     | a :: rem -> *)
(*         if Array.length a > 0 then concat_aux (Array.unsafe_get a 0) al else find_init rem *)
(*   in find_init al *)


(* let sub a ofs len = *)
(*   if ofs < 0 || len < 0 || ofs > Array.length a - len then invalid_arg "Array.sub" *)
(*   else if len = 0 then [||] *)
(*   else begin *)
(*     let r = Array.create len (Array.unsafe_get a ofs) in *)
(*     for i = 1 to len - 1 do Array.unsafe_set r i (Array.unsafe_get a (ofs + i)) done; *)
(*     r *)
(*   end *)

(* let fill a ofs len v = *)
(*   if ofs < 0 || len < 0 || ofs > Array.length a - len *)
(*   then invalid_arg "Array.fill" *)
(*   else for i = ofs to ofs + len - 1 do Array.unsafe_set a i v done *)

let show x = ()

let blit (a1: 'a array) (ofs1: int) (a2: 'a array) (ofs2: int) (len: int) =
  if len < 0 || ofs1 < 0 || ofs1 > Array.length a1 - len
             || ofs2 < 0 || ofs2 > Array.length a2 - len
  then invalid_arg "Array.blit"
  else if ofs1 < ofs2 then
    (* Top-down copy *)
    for i = len - 1 downto 0 do
      Array.unsafe_set a2 (ofs2 + i) (Array.unsafe_get a1 (ofs1 + i))
    done
  else
    (* Bottom-up copy *)
    for i = 0 to len - 1 do
      Array.unsafe_set a2 (ofs2 + i) (Array.unsafe_get a1 (ofs1 + i))
    done

(* let iter f a = *)
(*   for i = 0 to Array.length a - 1 do f(Array.unsafe_get a i) done *)

(* let map f a = *)
(*   let l = Array.length a in *)
(*   if l = 0 then [||] else begin *)
(*     let r = Array.create l (f(Array.unsafe_get a 0)) in *)
(*     for i = 1 to l - 1 do *)
(*       Array.unsafe_set r i (f(Array.unsafe_get a i)) *)
(*     done; *)
(*     r *)
(*   end *)

(* let iteri a f = *)
(*   for i = 0 to Array.length a - 1 do f i (Array.unsafe_get a i) done *)

(* let mapi a f = *)
(*   let l = Array.length a in *)
(*   if l = 0 then [||] else begin *)
(*     let r = Array.create l (f 0 (Array.unsafe_get a 0)) in *)
(*     for i = 1 to l - 1 do *)
(*       Array.unsafe_set r i (f i (Array.unsafe_get a i)) *)
(*     done; *)
(*     r *)
(*   end *)

(* let rec len l = *)
(*   match l with *)
(*       [] -> 0 *)
(*     | h::t -> 1 + len t *)

(* let to_list a = *)
(*   let rec tolist i res = *)
(*     if i < 0 then *)
(*       res *)
(*     else *)
(*       tolist (i - 1) (Array.unsafe_get a i :: res) in *)
(*   tolist (Array.length a - 1) ((fun x -> x) []) *)


(* (\* Cannot use List.length here because the List module depends on Array. *\) *)
(* let rec list_length accu = function *)
(*   | [] -> accu *)
(*   | h::t -> list_length (succ accu) t *)
(* ;; *)

(* let of_list l = match l with *)
(*     [] -> [||] *)
(*   | hd::tl -> *)
(*       let a = Array.create (len l) hd in *)
(*       let rec fill i l = match l with *)
(*           [] -> a *)
(*         | hd::tl -> *)
(*             let z = len tl in *)
(*               Array.unsafe_set a i hd; fill (i+1) tl in *)
(*       fill 1 tl *)

(* let fold_left f x a = *)
(*   let r = ref x in *)
(*   for i = 0 to Array.length a - 1 do *)
(*     r := f !r (Array.unsafe_get a i) *)
(*   done; *)
(*   !r *)

(* let fold_right f a x = *)
(*   let r = ref x in *)
(*   for i = Array.length a - 1 downto 0 do *)
(*     r := f (Array.unsafe_get a i) !r *)
(*   done; *)
(*   !r *)

(*
type 'a fakebottom = Bottom of int | Ok of 'a

let sort cmp a =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (get a i31) (get a (i31+1)) < 0 then x := i31+1;
      if cmp (get a !x) (get a (i31+2)) < 0 then x := i31+2;
      Ok !x
    end else
      if i31+1 < l && cmp (get a i31) (get a (i31+1)) < 0
      then Ok (i31+1)
      else if i31 < l then Ok i31 else Bottom i
  in
  let rec trickledown l i e =
    match maxson l i with Bottom i -> Bottom i | Ok j -> 
      if cmp (get a j) e > 0 then begin
        set a i (get a j);
        trickledown l j e
      end else begin
        set a i e;
        Ok ()
      end;
  in
  let rec trickle l i e = match trickledown l i e with Ok _ -> () | Bottom i -> set a i e in
  let rec bubbledown l i =
    match maxson l i with Bottom i -> Bottom i | Ok j -> 
    set a i (get a j);
    bubbledown l j
  in
  let bubble l i = match bubbledown l i with Ok _ -> (assert (0=1); assert false) | Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    let z = assert (i >= 0) in
    let z = assert (father >= 0) in
    assert (i <> father);
    if cmp (get a father) e < 0 then begin
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e;
    end else begin
      set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 do
    let e = (get a i) in
    set a i (get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e);
;;
*)

let cutoff = 5;;
let stable_sort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    (* note d - dstofs = (i1 - src1ofs) + (i2 - src2ofs) *)
    let rec loop i1 s1 i2 s2 d =
      let _ = show d in
      if cmp s1 s2 <= 0 then begin
        Array.unsafe_set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (Array.unsafe_get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        Array.unsafe_set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (Array.unsafe_get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (Array.unsafe_get a src1ofs) src2ofs (Array.unsafe_get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (Array.unsafe_get a (srcofs + i)) in
      let rec whileloop j =
	if j >= dstofs then begin
          if cmp (Array.unsafe_get dst j) e > 0 then begin
	    Array.unsafe_set dst (j + 1) (Array.unsafe_get dst j);
	    whileloop (j - 1)
          end else
            Array.unsafe_set dst (j + 1) e
        end else
	  Array.unsafe_set dst (j + 1) e
      in whileloop (dstofs + i - 1)
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 (Array.unsafe_get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

(* let fast_sort = stable_sort;; *)

