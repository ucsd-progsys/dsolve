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

exception Vec_index_out_of_bounds

let height = function
    Empty -> 0
  | Node(_,_,_,_,_,h) -> h

let length = function 
    Empty -> 0 
  | Node (_, cl, _, _, cr, _) -> 1 + cl + cr 

let makenode l d r =
  let (hl, cl) = match l with Empty -> (0,0) | Node(_,lcl,_,_,lcr,h) -> (h, lcl + lcr + 1) in 
  let (hr, cr) = match r with Empty -> (0,0) | Node(_,rcl,_,_,rcr,h) -> (h, rcl + rcr + 1) in 
  Node(l, cl, d, r, cr, (if hl >= hr then hl + 1 else hr + 1))

let rec create d = function 
    0 -> Empty 
  | n ->
      let ml = n / 2 in 
      let mr = n - ml - 1 in 
      makenode (create d ml) d (create d mr)

(* bal assumes that l and r are of similar height *)
let bal l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Vec.bal"
    | Node(ll, _, ld, lr, _, _) ->
        if height ll >= height lr then
          makenode ll ld (makenode lr d r)
        else begin
          match lr with
            Empty -> invalid_arg "Vec.bal"
          | Node(lrl, _, lrd, lrr, _, _) ->
              makenode (makenode ll ld lrl) lrd (makenode lrr d r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Vec.bal"
    | Node(rl, _, rd, rr, _, _) ->
        if height rr >= height rl then
          makenode (makenode l d rl) rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Vec.bal"
          | Node(rll, _, rld, rlr, _, _) ->
              makenode (makenode l d rll) rld (makenode rlr rd rr)
        end
  end 
  else makenode l d r 
      
(* This is a recursive version of balance, which balances a tree all the way down. 
   The trees l and r can be of any height, but they need to be internally balanced.  
   Useful to implement concat. *)
let rec recbal l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Vec.bal"
    | Node(ll, _, ld, lr, _, _) ->
        if height ll >= height lr then
          makenode ll ld (recbal lr d r)
        else begin
          match lr with
            Empty -> invalid_arg "Vec.bal"
          | Node(lrl, _, lrd, lrr, _, _) ->
              makenode (makenode ll ld lrl) lrd (recbal lrr d r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Vec.bal"
    | Node(rl, _, rd, rr, _, _) ->
        if height rr >= height rl then
          makenode (recbal l d rl) rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Vec.bal"
          | Node(rll, _, rld, rlr, _, _) ->
              makenode (recbal l d rll) rld (makenode rlr rd rr)
        end
  end 
  else makenode l d r 
      
let empty = Empty
  
let is_empty = function Empty -> true | _ -> false

let singleton d = Node (Empty, 0, d, Empty, 0, 1)

let rec get i = function 
    Empty -> raise Vec_index_out_of_bounds
  | Node (l, cl, d, r, cr, _) -> 
      if i < cl then get i l 
      else if i > cl then get (i - cl - 1) r 
      else d 

let rec set i d = function 
    Empty -> raise Vec_index_out_of_bounds
  | Node (l, cl, dd, r, cr, _) -> 
      if i < cl then makenode (set i d l) dd r  
      else if i > cl then makenode l dd (set (i - cl - 1) d r)
      else makenode l d r 

let rec append d = function 
    Empty -> Node (Empty, 0, d, Empty, 0, 1)
  | Node (l, _, dd, r, _, _) -> 
      bal l dd (append d r)
  
let setappend d0 d i v =
  let l = length v in 
  if l > i then set i d v 
  else begin
    let vr = ref v in 
    for j = l to i - 1 do 
      vr := append d0 !vr 
    done; 
    append d !vr
  end 

let rec leftmost = function
    Empty -> raise Vec_index_out_of_bounds
  | Node(Empty, _, d, r, _, _) -> d
  | Node(l, _, d, r, _, _) -> leftmost l
      
let rec remove_leftmost = function
    Empty -> invalid_arg "Vec.remove_min_elt"
  | Node(Empty, _, d, r, _, _) -> r
  | Node(l, _, d, r, _, _) -> bal (remove_leftmost l) d r
      
(* merge uses bal, not recbal, so it only works for trees of similar height *)
let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
      let d = leftmost t2 in
      bal t1 d (remove_leftmost t2)
	
(* Concat works also for t1 and t2 of very different heights *)
let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
      let d = leftmost t2 in
      recbal t1 d (remove_leftmost t2)
	
let rec pop i = function
    Empty -> raise Vec_index_out_of_bounds
  | Node(l, cl, d, r, cr, h) ->
      if i < cl then 
	let (e, v) = pop i l in 
	(e, bal v d r)
      else if i > cl then 
	let (e, v) = pop (i - cl - 1) r in 
	(e, bal l d v)
      else (d, merge l r)

let rec remove i = function 
    Empty -> raise Vec_index_out_of_bounds
  | Node(l, cl, d, r, cr, h) ->
      if i < cl then 
	bal (remove i l) d r 
      else if i > cl then 
	bal l d (remove (i - cl - 1) r)
      else merge l r 

	  
let rec insert i d = function 
    Empty -> begin
      if i = 0 
      then Node (Empty, 0, d, Empty, 0, 1)
      else raise Vec_index_out_of_bounds
    end
  | Node(l, cl, dd, r, cr, h) -> 
      if i < cl then bal (insert i d l) dd r 
      else if i > cl then bal l dd (insert (i - cl - 1) d r)
      else bal l d (insert 0 dd r)

let rec sub i j v = match v with 
    Empty -> Empty
  | Node (l, cl, dd, r, cr, _) -> 
      if i >= j then Empty
	(* Important for sharing *)
      else if i <= 0 && j >= cl + cr + 1 then v 
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

let rec iteri f v = 
  let rec offsetiteri f k = function
    Empty -> ()
  | Node(l, cl, d, r, _, _) ->
      offsetiteri f k l; f (k + cl) d; offsetiteri f (k + cl + 1) r
  in offsetiteri f 0 v

let rec reviter f = function
    Empty -> ()
  | Node(l, _, d, r, _, _) ->
      iter f r; f d; iter f l

let rec rangeiter f i j = function
    Empty -> ()
  | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if i < cl && j > 0 then rangeiter f i j l; 
	if i <= cl && j > cl then f d;
	if j > cl + 1 && i <= cl + cr + 1 then rangeiter f (i - cl - 1) (j - cl - 1) r
      end

let rec revrangeiter f i j = function
    Empty -> ()
  | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if j > cl + 1 && i <= cl + cr + 1 then rangeiter f (i - cl - 1) (j - cl - 1) r;
	if i <= cl && j > cl then f d;
	if i < cl && j > 0 then rangeiter f i j l
      end

let rangeiteri f i j v = 
  let rec offsetrangeiteri f k i j = function
      Empty -> ()
    | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if i < cl && j > 0 then offsetrangeiteri f k i j l; 
	if i <= cl && j > cl then f (k + cl) d;
	if j > cl + 1 && i <= cl + cr + 1 
	then offsetrangeiteri f (k + cl + 1) (i - cl - 1) (j - cl - 1) r
      end
  in offsetrangeiteri f 0 i j v 

let revrangeiteri f i j v = 
  let rec offsetrevrangeiteri f k i j = function
      Empty -> ()
    | Node(l, cl, d, r, cr, _) ->
      if i < j then begin 
	if j > cl + 1 && i <= cl + cr + 1 
	then offsetrevrangeiteri f (k + cl + 1) (i - cl - 1) (j - cl - 1) r;
	if i <= cl && j > cl then f (k + cl) d;
	if i < cl && j > 0 then offsetrevrangeiteri f k i j l
      end
  in offsetrevrangeiteri f 0 i j v 

let rec map f = function
    Empty -> Empty
  | Node(l, cl, d, r, cr, h) -> Node(map f l, cl, f d, map f r, cr, h)
      
let mapi f v = 
  let rec offsetmapi f k = function 
      Empty -> Empty
    | Node(l, cl, d, r, cr, h) -> 
	Node(offsetmapi f k l, cl, f (k + cl) d, offsetmapi f (k + cl + 1) r, cr, h)
  in offsetmapi f 0 v 

let rec fold f v accu =
  match v with
    Empty -> accu
  | Node(l, _, d, r, _, _) ->
      fold f r (f d (fold f l accu))
	
let foldi f v accu =
  let rec offsetfoldi f k v accu = 
    match v with
      Empty -> accu
    | Node(l, cl, d, r, _, _) ->
	offsetfoldi f (k + cl + 1) r (f (k + cl) d (offsetfoldi f k l accu))
  in offsetfoldi f 0 v accu
	
let rangefoldi f i j v accu = 
  let rec offsetrangefoldi f k i j v accu = 
    match v with 
      Empty -> accu
    | Node (l, cl, d, r, cr, _) -> 
	if i >= j then accu
	else begin 
	  let al = if i < cl && j > 0 then offsetrangefoldi f k i j l accu else accu in 
	  let ad = if i <= cl && j > cl then f (cl + k) d al else al in 
	  if j > cl + 1 && i <= cl + cr + 1
	  then offsetrangefoldi f (k + cl + 1) (i - cl - 1) (j - cl - 1) r ad
	  else ad
	end
  in offsetrangefoldi f 0 i j v accu 

let revfoldi f v accu =
  let rec offsetrevfoldi f k v accu = 
    match v with
      Empty -> accu
    | Node(l, cl, d, r, _, _) ->
	offsetrevfoldi f k l (f (k + cl) d (offsetrevfoldi f (k + cl + 1) r accu))
  in offsetrevfoldi f 0 v accu

let revrangefoldi f i j v accu = 
  let rec offsetrevrangefoldi f k i j v accu = 
    match v with 
      Empty -> accu
    | Node (l, cl, d, r, cr, _) -> 
	if i >= j then accu
	else begin 
	  let ar = if j > cl + 1 && i <= cl + cr + 1
	  then offsetrevrangefoldi f (k + cl + 1) (i - cl - 1) (j - cl - 1) r accu
	  else accu
	  in 
	  let ad = if i <= cl && j > cl then f (cl + k) d ar else ar in 
	  if i < cl && j > 0 then offsetrevrangefoldi f k i j l ad else ad
	end
  in offsetrevrangefoldi f 0 i j v accu 

let rec of_list = function 
    [] -> Empty 
  | d :: l -> insert 0 d (of_list l)

let to_list v = 
  let rec auxtolist accu = function 
      Empty -> accu 
    | Node (l, _, d, r, _, _) -> auxtolist (d :: auxtolist accu r) l 
  in auxtolist [] v;;

let rec to_array w = 
  match w with 
    Empty -> [||]
  | Node (l, cl, d, r, cr, _) -> 
      begin 
	(* Creates the array *)
	let n = cl + cr + 1 in 
	let a = Array.make n d in 
	(* and fills it *)
	let rec fill a k = function 
	    Empty -> a
	  | Node (l, cl, d, r, _, _) -> begin
	      ignore (fill a k l); 
	      Array.set a (k + cl) d; 
	      fill a (k + cl + 1) r
	    end
	in fill a 0 w 
      end
	      
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
