(* Vec: implementation of extensible arrays. 

   Copyright Luca de Alfaro <lda@dealfaro.com>, 2007.

   Version 1.1

   Based on Xavier Leroy's code for Set and Map.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the following special exception:

   You may link, statically or dynamically, a "work that uses the
   Library" with a publicly distributed version of the Library to
   produce an executable file containing portions of the Library, and
   distribute that executable file under terms of your choice, without
   any of the additional requirements listed in clause 6 of the GNU
   Library General Public License.  By "a publicly distributed version
   of the Library", we mean either the unmodified Library as
   distributed by INRIA, or a modified version of the Library that is
   distributed under the conditions defined in clause 2 of the GNU
   Library General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be
   covered by the GNU Library General Public License.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   The GNU Library General Public License is available at
   http://www.gnu.org/copyleft/lgpl.html; to obtain it, you can also
   write to the Free Software Foundation, Inc., 59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.
 *)
type 'a t =
    Empty
      (* meaning: node of left el, left n.els, element, right el, right n. els, heigth *)
  | Node of 'a t * int * 'a * 'a t * int * int

(*exception Vec_index_out_of_bounds*)

let height t =
  match t with
    Empty -> 0
  | Node(_, _, _, _, _, h) -> h

let length t =
  match t with
    Empty -> 0
  | Node (_, cl, _, _, cr, _) -> 1 + cl + cr

  
let makenode l d r =
  let (hl, cl) = match l with
                    Empty -> (0,0)
                  | Node(_,lcl,_,_,lcr,h) -> (h, lcl + lcr + 1) in
  let (hr, cr) = match r with
                    Empty -> (0,0)
                  | Node(_,rcl,_,_,rcr,h) -> (h, rcl + rcr + 1) in
  Node(l, cl, d, r, cr, (if hl >= hr then hl + 1 else hr + 1))


let rec create d n =
    if n = 0 then Empty else
      let ml = n / 2 in 
      let mr = n - ml - 1 in 
      let l = create d ml in
      let r = create d mr in (* defer this particular property to runtime *)
      if height l >= height r + 3 or height l <= height r - 3 then
        assert false 
      else
        makenode l d r


(* bal assumes that l and r are of similar height *)
let bal l d r =
  let hl = 
    match l with 
      Empty -> 0
    | Node(_,_,_,_,_,h) -> h in
  let hr = 
    match r with 
      Empty -> 0 
    | Node(_,_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
    | Node(ll, lll, ld, lr, llr, h) ->
        if height ll >= height lr then
          makenode ll ld (makenode lr d r)
        else begin
          match lr with
            Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
          | Node(lrl, llrl, lrd, lrr, llrr, h) ->
              makenode (makenode ll ld lrl) lrd (makenode lrr d r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
    | Node(rl, lrl, rd, rr, lrr, h) ->
        if height rr >= height rl then
          makenode (makenode l d rl) rd rr
        else begin
          match rl with
            Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
          | Node(rll, lrll, rld, rlr, lrlr, h) ->
              makenode (makenode l d rll) rld (makenode rlr rd rr)
        end
  end 
  else makenode l d r

  (* recbal as originally included in vec.ml did not provide a balancing guarantee. 
   * this has been fixed below by adding at worst a small constant number of rotations per level.
   * the fixed version also typechecks. *)

  
(* This is a recursive version of balance, which balances a tree all the way down. 
   The trees l and r can be of any height, but they need to be internally balanced.  
   Useful to implement concat. *)
let rec recbal l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
    | Node(ll, _, ld, lr, _, h) ->
        if height ll >= height lr then
          bal ll ld (recbal lr d r)
        else begin
          match lr with
            Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
          | Node(lrl, _, lrd, lrr, _, h) ->
              let nr = recbal lrr d r in
                if height nr <= height lr - 3 then
                  makenode ll ld (bal lrl lrd nr)
                else
                  makenode (makenode ll ld lrl) lrd nr
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
    | Node(rl, _, rd, rr, _, h) ->
        if height rr >= height rl then
            bal (recbal l d rl) rd rr
        else begin
          match rl with
            Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.bal"*)
          | Node(rll, _, rld, rlr, _, h) ->
              let nl = recbal l d rll in
                if height nl <= height rl - 3 then
                  makenode (bal nl rld rlr) rd rr
                else
                  makenode nl rld (makenode rlr rd rr)
        end
  end 
  else makenode l d r


let empty = Empty
  
let is_empty t = 
  match t with
  | Empty -> true
  | Node(_, _, _, _, _, _) -> false


let singleton d = Node (Empty, 0, d, Empty, 0, 1)


let rec get i t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
  | Node (l, cl, d, r, cr, _) -> 
      if i < cl then get i l 
      else if i > cl then get (i - cl - 1) r 
      else d 

      
let rec set i d t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
  | Node (l, cl, dd, r, cr, _) -> 
      if i < cl then makenode (set i d l) dd r
      else if i > cl then makenode l dd (set (i - cl - 1) d r)
      else makenode l d r


let rec append d t =
  match t with
    Empty -> Node (Empty, 0, d, Empty, 0, 1)
  | Node (l, ll, dd, r, lr, h) -> 
      bal l dd (append d r)

let show x = x
      
(* although it's not obvious from setappend, length v > 0 due to dep on set *)
let setappend d0 d i v =
  let l = length v in 
  if l > i then set i d v 
  else begin
    let rec app_rec n v =
      if n = (l-1) then v else append d0 (app_rec (n-1) v) in 
    append d (app_rec (i-1) v)
  end 


 
let rec leftmost t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
  | Node(l, ll', d, r, lr', h) -> 
      match l with
      | Empty -> d
      | Node(ll, lll, d', lr, llr, h') -> leftmost l
      
      
let rec remove_leftmost t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*invalid_arg "Vec.remove_min_elt"*)
  | Node(l, ll, d, r, lr, h) ->
      match l with
      | Empty -> r
      | Node(ll, lll, ld, lr, llr, h') -> bal (remove_leftmost l) d r

      
(* merge uses bal, not recbal, so it only works for trees of similar height *)
let merge t1 t2 =
  match t1 with
  | Empty -> t2
  | Node(_, _, _, _, _, _) ->
      match t2 with
      | Empty -> t1
      | Node(_, _, _, _, _, _) ->
          let d = leftmost t2 in
          bal t1 d (remove_leftmost t2)
          
          

(* Concat works also for t1 and t2 of very different heights *)
let concat t1 t2 =
  match t1 with
  | Empty -> t2
  | Node(_, _, _, _, _, _) ->
      match t2 with
      | Empty -> t1
      | Node(_, _, _, _, _, _) ->
          let d = leftmost t2 in
          recbal t1 d (remove_leftmost t2)


let rec pop i t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
  | Node(l, cl, d, r, cr, h) ->
      if i < cl then 
	let (e, v) = pop i l in 
	(e, bal v d r)
      else if i > cl then 
	let (e, v) = pop (i - cl - 1) r in 
	(e, bal l d v)
      else (d, merge l r)


let rec remove i t =
  match t with
    Empty -> let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
  | Node(l, cl, d, r, cr, h) ->
      if i < cl then 
	bal (remove i l) d r 
      else if i > cl then 
	bal l d (remove (i - cl - 1) r)
      else merge l r 

  
let rec insert i d t =
  match t with
    Empty -> begin
      if i = 0 
      then Node (Empty, 0, d, Empty, 0, 1)
      else let _ = assert (1 = 0) in assert false (*raise Vec_index_out_of_bounds*)
    end
  | Node(l, cl, dd, r, cr, h) -> 
      if i < cl then bal (insert i d l) dd r 
      else if i > cl then bal l dd (insert (i - cl - 1) d r)
      else bal l d (insert 0 dd r)
      


let rec sub i j t = 
  match t with 
    Empty -> Empty
  | Node (l, cl, dd, r, cr, _) -> 
      if i >= j then Empty
	(* Important for sharing *)
      else if i <= 0 && j >= cl + cr + 1 then t
      else begin 
	if j <= cl then sub i j l 
	else if j = cl + 1 then append dd (sub i cl l)
	else if i = cl then insert 0 dd (sub 0 (j - cl - 1) r)
	else if i > cl then sub (i - cl - 1) (j - cl - 1) r
	else begin
	  (* dd straddles the interval *)
	  let ll = sub i cl l in 
	  let rr = sub 0 (j - cl - 1) r in 
	  recbal ll dd rr
	end
      end

let rec iter f = function
    Empty -> ()
  | Node(l, _, d, r, _, _) ->
      iter f l; f d; iter f r


let rec iteri t f = 
  let rec offsetiteri t' k =
    match t' with
      Empty -> ()
    | Node(l, cl, d, r, _, _) ->
        offsetiteri l k;
        f (k + cl) d;
        offsetiteri r (k + cl + 1)
  in offsetiteri t 0


(*
let rec reviter f = function
    Empty -> ()
  | Node(l, _, d, r, _, _) ->
      reviter f r; f d; reviter f l

let rec rangeiter t f i j =
  match t with
    Empty -> ()
  | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if i < cl && j > 0 then rangeiter l f i j else (); 
	if i <= cl && j > cl then f d else ();
	if j > cl + 1 && i <= cl + cr + 1 then rangeiter r f (i - cl - 1) (j - cl - 1) else ()
      end

(* another bug in revrangeiter *)
let rec revrangeiter t f i j =
  match t with
    Empty -> ()
  | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if j > cl + 1 && i <= cl + cr + 1 then revrangeiter r f (i - cl - 1) (j - cl - 1);
	if i <= cl && j > cl then f d;
	if i < cl && j > 0 then revrangeiter l f i j
      end
*)
let rangeiteri i j t f  = 
  let rec offsetrangeiteri k i' j' t' = 
    match t' with
      Empty -> ()
    | Node(l, cl, d, r, cr, _) ->
      if i' < j' then begin 
         if i' < cl && j' > 0 then offsetrangeiteri k i' j' l else (); 
         if i' <= cl && j' > cl then f (k + cl) d else ();
         if j' > cl + 1 && i' <= cl + cr + 1 then offsetrangeiteri (k + cl + 1) (i' - cl - 1) (j' - cl - 1) r else ()
      end else ()
  in offsetrangeiteri 0 i j t 

let revrangeiteri i j t f = 
  let rec offsetrevrangeiteri k i j t' =
    match t' with
      Empty -> ()
    | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if j > cl + 1 && i <= cl + cr + 1 
	then offsetrevrangeiteri (k + cl + 1) (i - cl - 1) (j - cl - 1) r else ();
	if i <= cl && j > cl then f (k + cl) d else ();
	if i < cl && j > 0 then offsetrevrangeiteri k i j l else ()
      end else ()
  in offsetrevrangeiteri 0 i j t 

let mapi t f = 
  let rec offsetmapi k t' =
    match t' with
      Empty -> Empty
    | Node(l, cl, d, r, cr, h) -> 
	Node(offsetmapi k l, cl, f (k + cl) d, offsetmapi (k + cl + 1) r, cr, h)
  in offsetmapi 0 t
	
let foldi t f accu =
  let rec offsetfoldi k t' accu = 
    match t' with
      Empty -> accu
    | Node(l, cl, d, r, _, _) ->
	offsetfoldi (k + cl + 1) r (f (k + cl) d (offsetfoldi k l accu))
  in offsetfoldi 0 t accu
	


let rangefoldi i j t f accu = 
  let rec offsetrangefoldi k i j t' accu = 
    match t' with 
      Empty -> accu
    | Node (l, cl, d, r, cr, _) -> 
	if i >= j then accu
	else begin 
	  let al = if i < cl && j > 0 then offsetrangefoldi k i j l accu else accu in 
	  let ad = if i <= cl && j > cl then f (cl + k) d al else al in 
	  if j > cl + 1 && i <= cl + cr + 1
	  then offsetrangefoldi (k + cl + 1) (i - cl - 1) (j - cl - 1) r ad
	  else ad
	end
  in offsetrangefoldi 0 i j t accu 

let revfoldi t f accu =
  let rec offsetrevfoldi k t' accu = 
    match t' with
      Empty -> accu
    | Node(l, cl, d, r, _, _) ->
	offsetrevfoldi k l (f (k + cl) d (offsetrevfoldi (k + cl + 1) r accu))
  in offsetrevfoldi 0 t accu

let revrangefoldi i j t f accu = 
  let rec offsetrevrangefoldi k i j t' accu = 
    match t' with 
      Empty -> accu
    | Node (l, cl, d, r, cr, _) -> 
	if i >= j then accu
	else begin 
	  let ar = if j > cl + 1 && i <= cl + cr + 1
	  then offsetrevrangefoldi (k + cl + 1) (i - cl - 1) (j - cl - 1) r accu
	  else accu
	  in 
	  let ad = if i <= cl && j > cl then f (cl + k) d ar else ar in 
	  if i < cl && j > 0 then offsetrevrangefoldi k i j l ad else ad
	end
  in offsetrevrangefoldi 0 i j t accu 

(*
let rec map f = function
  | Empty -> Empty
  | Node(l, cl, d, r, cr, h) -> Node(map f l, cl, f d, map f r, cr, h)

let rec fold f v accu =
  match v with
    Empty -> accu
  | Node(l, _, d, r, _, _) ->
      fold f r (f d (fold f l accu))

let rec of_list = function 
    [] -> Empty 
  | d :: l -> insert 0 d (of_list l)

let to_list v = 
  let rec auxtolist accu = function 
      Empty -> accu 
    | Node (l, _, d, r, _, _) -> auxtolist (d :: auxtolist accu r) l 
  in auxtolist [] v;;
*)

let rec to_array t = 
  match t with 
    Empty -> [||]
  | Node (l, cl, d, r, cr, _) -> 
      begin 
	(* Creates the array *)
	let n = (cl + cr + 1) in 
	let a = Array.make n d in 
	(* and fills it *)
	let rec fill k t' =
    match t' with
	    Empty -> a
	  | Node (l, cl, d, r, _, _) -> begin
	      ignore (fill k l); 
	      Array.set a (k + cl) d; 
	      fill (k + cl + 1) r
	    end
	in fill 0 t 
      end
	      
(*
let of_array a =
  let f accu el = append el accu in 
  Array.fold_left f Empty a;; 

(* Visitor paradigm *)

(* Post-order visitor *)
let visit_post ve vn a = 
  let rec f = function
      Empty -> ve
    | Node (l, _, d, r, _, _) -> 
	let rl = f l in 
	let rr = f r in 
	vn rl d rr
  in f a;;

(* In-order visitor *)
let visit_in ve vl vr a = 
  let rec f = function 
      Empty -> ve
    | Node (l, _, d, r, _, _) ->
	let rl = vl (f l) d in 
	vr rl (f r)
  in f a;; 

(* Unit testing *)

if false then begin 
  let print_vec v = 
    let p i = Printf.printf " %d" i in 
    Printf.printf "["; iter p v; Printf.printf " ]\n"
  in
  let v = of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19] in 
  print_vec v; 
  print_vec (of_array (to_array v));
  Printf.printf "%d\n" (get 5 v); 
  let u = concat v v in
  Printf.printf "%d\n" (length u); 
  Printf.printf "\n"; 
  print_vec (sub 4 9 (set 5 100 v)); 
  print_vec (sub (-10) 43 v); 
  print_vec (sub 30 30 v); 
  print_vec (sub 1 11 v); 
  let mult2 x = 2 * x in 
  print_vec (map mult2 (sub 3 8 v));
  let ff i d acc = (string_of_int i) ^ ":" ^ (string_of_int d) ^ ", " ^ acc in 
  Printf.printf "%s\n" (revrangefoldi ff 5 13 v "");
  print_vec (setappend (-2) (-1) 16 (sub 4 12 v))

end;;
*)


