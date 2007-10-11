qualif LTs(x) : x < size arr;;
qualif LEs(x) : x <= size arr;;
qualif NEs(x) : not(x = size arr);;
qualif EQs(x) : size x = size arr;;
qualif LTs1(x) : x < size _1_arr;;
qualif LEs1(x) : x <= size _1_arr;;
qualif NEs1(x) : not(x = size _1_arr);;
qualif EQs1(x) : size x = size _1_arr;;
qualif LTs2(x) : x < size yarr;;
qualif LEs2(x) : x <= size yarr;;
qualif NEs2(x) : not(x = size yarr);;
qualif EQs2(x) : size x = size yarr;;
qualif LTs3(x) : x < size _2_arr;;
qualif LEs3(x) : x <= size _2_arr;;
qualif NEs3(x) : not(x = size _2_arr);;
qualif EQs3(x) : size x = size _2_arr;;
qualif SGT0(x) : 0 < size x ;;

qualif SEQ(x) : size x = size yarr;;

qualif LT0(x) : x < 0;;
qualif LE0(x) : x <= 0;;
qualif NE0(x) : not(x = 0);;
qualif GT0(x) : 0 < x;;
qualif GE0(x) : 0 <= x;;
qualif BOOL(x) : (x = 0) or (x = 1);;
qualif TRUE(x) : x = 1;;
qualif FALSE(x) : x = 0;;

qualif LTsz(x) : x < sz;;
qualif LEsz(x) : x <= sz;;
qualif NEsz(x) : not(x = sz);; 

let
sortRange arr start n =
  let item _1_i = get arr _1_i in
  let swap _2_i _1_j =
    let _1_tmp = item _2_i in set arr _2_i (item _1_j); set arr _1_j _1_tmp
  in
  let rec vecswap _3_i _2_j _1_n = 
		let _3_i_plus = _3_i + 1 in
		let _2_j_plus = _2_j + 1 in
		let _1_n_minus = _1_n - 1 in
		if _1_n = 0 then () else begin swap _3_i _2_j; vecswap _3_i_plus _2_j_plus _1_n_minus end
  in

  let insertSort start _2_n =
    let limit = start + _2_n in
		let start_plus = start + 1 in
    let rec outer _4_i  =
			let _4_i_plus = _4_i + 1 in
      if limit < _4_i_plus then ()
      else let rec inner _3_j =
						 let _3_j_minus = _3_j - 1 in
             if _3_j < start_plus then outer _4_i_plus else
								let i_3_j_minus = item _3_j_minus in
									let i_3_j = item _3_j in
										if _3_j < _3_j_minus then (swap _3_j _3_j_minus; inner _3_j_minus) else outer _4_i_plus
		       in inner _4_i
    in outer start_plus
  in insertSort start n
in
let sorting _2_arr = (sortRange _2_arr 0 (size _2_arr) ; _2_arr) in
(* sorted checks if a list is well-sorted -- i never would have figured this out if not for the comment *)
let
:orted _1_arr =
  let len = size _1_arr in
  let rec s v _5_i =
		let _5_i_plus = _5_i + 1 in
    let v' = get _1_arr _5_i in
			if v' < v then false else 
				(if _5_i_plus = len then true else s v' _5_i_plus)
  in
  if len < 2 then true else s (get _1_arr 0) 1
in
let sz = get_pos_int () in
let sz_plus = sz + 5 in
let sz_minus = sz_plus - 1 in
let yarr = make (get_pos_int ()) 0 in
(sortRange yarr 0 sz_minus; sorted yarr)
;;

(*type order = LESS | EQUAL | GREATER
;;

let{size:nat}
sortRange(arr, start, n, cmp) =
  let item i = arr..(i) withtype {i:nat | i < size } int(i) -> 'a in
  let swap (i,j) =
    let tmp = item i in arr..(i) <- item j; arr..(j) <- tmp
  withtype {i:nat}{j:nat | i < size /\ j < size } int(i) * int(j) -> unit in
  let rec vecswap (i,j,n) = if eq_int n 0 then () else begin swap(i,j); vecswap(i+1,j+1,n-1) end
  withtype {i:nat}{j:nat}{n:nat | i+n <= size /\ j+n <= size } int(i) * int(j) * int(n) -> unit in

  let insertSort (start, n) =
    let limit = start+n in
    let rec outer i =
      if ge_int i limit then ()
      else let rec inner j =
             if le_int j start then outer(i+1)
             else let j' = j - 1 in
               match cmp(item j',item j) with
                 GREATER -> (swap(j,j'); inner j')
               | _  -> outer(i+1)
           withtype {j:nat | j < size } int(j) -> unit in inner i
    withtype {i:nat} int(i) -> unit in outer(start+1)
  withtype {start:nat}{n:nat | start+n <= size } int(start) * int(n) -> unit in
  insertSort (start, n)
withtype {start:nat}{n:nat | start+n <= size }
         'a vect(size) * int(start) * int(n) * ('a * 'a -> order) -> unit
;;

let sorting arr cmp = sortRange(arr, 0, vect_length arr, cmp); arr
withtype {size:nat} 'a vect(size) -> ('a * 'a -> order) -> 'a vect(size)
;;

(* sorted checks if a list is well-sorted *)
let{size:nat}
sorted cmp arr =
  let len = vect_length arr in
  let rec s (v,i) =
    let v' = arr..(i) in
      match cmp(v,v') with
        GREATER -> false
      | _ -> if eq_int (i+1) len then true else s(v',i+1)
  withtype {i:nat | i < size } 'a * int(i) -> bool in
  if le_int len 1 then true else s(arr..(0),1)
withtype ('a * 'a -> order) -> 'a vect(size) -> bool
;;*)
