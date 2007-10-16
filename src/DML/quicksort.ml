


(*
 * This is an example showing that array bounds checking
 * is not needed for doing quicksort on an array.
 * The code is copied from SML/NJ lib with some modification.
 *)

let min _1_m _1_n = 
	let _1_n_plus = _1_n + 1 in 
	if _1_m < _1_n_plus then _1_m else _1_n
in 
let sortRange _1_arr _1_start _1_n =
  let item _1_i = Array.get _1_arr _1_i in
  let swap _2_i _2_j =
	let i_2_j = item _2_j in
    let tmp = item _2_i in (Array.set _1_arr _2_i i_2_j; Array.set _1_arr _2_j tmp)
  in
  let rec vecswap _3_i _3_j _3_n = 
		let _3_i_plus = _3_i + 1 in
		let _3_j_plus = _3_j + 1 in
		let _3_n_minus = _3_n - 1 in
		if _3_n = 0 then () else begin swap _3_i _3_j; vecswap _3_i_plus _3_j_plus _3_n_minus end
  in 

  (* insertSort is called if there are less than 8 elements to be sorted *)
  let insertSort _2_start _4_n =
    let limit = _2_start + _4_n in
		let _2_start_plus = _2_start + 1 in
    let rec outer _4_i =
			let _4_i_plus = _4_i + 1 in
      if limit < _4_i_plus then ()
      else let rec inner _4_j =
             if _4_j < _2_start_plus then outer _4_i_plus 
             else let _4_j' = _4_j - 1 in
								  let ij' = item _4_j' in
									let ij = item _4_j in
									if ij < ij' then
										(swap _4_j _4_j'; inner _4_j') else
										outer _4_i_plus
           in inner _4_i
    in outer _2_start_plus
  in 

  (* calculate the median of three *)
  let med3 _5_a _5_b _5_c =
    let _5_a' = item _5_a in
		let _5_b' = item _5_b in 
		let _5_c' = item _5_c in
		let lt_ab = _5_a' < _5_b' in
		let lt_bc = _5_b' < _5_c' in
		let gt_bc = _5_c' < _5_b' in
		let lt_ac = _5_a' < _5_c' in
		if lt_ab && lt_bc then
			_5_b else
		if lt_ab then
			if lt_ac then _5_c else _5_a else
		if gt_bc then _5_b else
		if lt_ac then _5_a else _5_c
	in 

  (* generate the pivot for splitting the elements *)
  let getPivot _6_a _6_n =
		let _6_a_plus_n = _6_a + _6_n in
    if _6_n < 8 then _6_a_plus_n / 2
    else let _6_p1 = _6_a in
				 let _6_pm = _6_a_plus_n / 2 in
				 let _6_pn = _6_a_plus_n - 1 in
       	 if _6_n < 41 then med3 _6_p1 _6_pm _6_pn else
      		 let _6_d = _6_n / 8 in
					 let _2_6_d = 2 * _6_d in
					 let _6_p1_plus_d = _6_p1 + _6_d in
					 let _6_p1_plus_2_d = _6_p1 + _2_6_d in
					 let _6_pm_minus_d = _6_pm - _6_d in
					 let _6_pm_plus_d = _6_pm + _6_d in
					 let _6_pn_minus_2_d = _6_pn - _2_6_d in
					 let _6_pn_minus_d = _6_pn - _6_d in
           let new_6_p1 = med3 _6_p1 _6_p1_plus_d _6_p1_plus_2_d in
	   			 let new_6_pm = med3 _6_pm_minus_d _6_pm _6_pm_plus_d in
	   			 let new_6_pn = med3 _6_pn_minus_2_d _6_pn_minus_d _6_pn in 
							med3 new_6_p1 new_6_pm new_6_pn
	in	

  let rec quickSort _7_a _7_n =
		let _7_arg = (_7_a, _7_n) in
    let rec bottom _8_limit _8_pa _8_pb = 
			let _8_arg = (_8_pa, _8_pb) in
      if _8_limit < _8_pb then _8_arg else
			let _8_ipb = item _8_pb in
			let _7_ia = item _7_a in
			if _7_ia < _8_ipb then _8_arg else
				let _8_pb_plus = _8_pb + 1 in
				let _8_pa_plus = _8_pa + 1 in
				if _7_ia < _8_ipb then bottom _8_limit _8_pa _8_pb_plus else
						(swap _7_a _7_n; bottom _8_limit _8_pa_plus _8_pb_plus)
		in 
    let rec top _9_limit _9_pc _9_pd = 
			let _9_arg = (_9_pc, _9_pd) in
			let _9_ipc = item _9_pc in
			let _7_ia = item _7_a in
			let _9_pc_minus = _9_pc - 1 in
			let _9_pd_minus = _9_pd - 1 in
      if _9_pc < _9_limit then _9_arg else
      if _9_ipc < _7_ia then _9_arg else
			if _7_ia < _9_ipc then top _9_limit _9_pc_minus _9_pd else
			(swap _7_a _7_n; top _9_limit _9_pc_minus _9_pd_minus) 
    in 
    let rec split _10_pa _10_pb _10_pc _10_pd =
			let _10_bot = bottom _10_pc _10_pa _10_pb in
			let new_10_pa = fst _10_bot in
			let new_10_pb = snd _10_bot in
			let _10_top = top _10_pb _10_pc _10_pd in
			let new_10_pc = fst _10_top in
			let new_10_pd = snd _10_top in
			let new_10_pb_plus = new_10_pb + 1 in
			let new_10_pc_minus = new_10_pc - 1 in
			let _10_arg = (new_10_pa, (new_10_pb, (new_10_pc, new_10_pd))) in
      if new_10_pc < new_10_pb+1 then _10_arg
      else begin swap new_10_pb new_10_pc; 
								 split new_10_pa new_10_pb_plus new_10_pc_minus new_10_pd end
 	  in 

    let _7_pm = getPivot _7_a _7_n in
    let __7_none = swap _7_a _7_pm in
    let _7_a_plus = _7_a + 1 in
    let _7_a_plus_7_n = _7_a + _7_n in
		let _7_a_plus_7_n_minus = _7_a_plus_7_n - 1 in
    let pa_pb_pc_pd = split _7_a_plus _7_a_plus _7_a_plus_7_n_minus _7_a_plus_7_n_minus in
		let _7_pa = fst pa_pb_pc_pd in
		let _7_tmp1 = snd pa_pb_pc_pd in
		let _7_pb = fst _7_tmp1 in
		let _7_tmp2 = snd _7_tmp1 in
		let _7_pc = fst _7_tmp2 in
		let _7_pd = snd _7_tmp2 in 
		
    let _7_pn = _7_a + _7_n in

		let _7_pa_minus_7_a = _7_pa - _7_a in
		let _7_pb_minus_7_pa = _7_pb - _7_pa in

    let r = min _7_pa_minus_7_a _7_pb_minus_7_pa in

		let _7_pb_minus_r = _7_pb - r in

    let __8_none = vecswap _7_a _7_pb_minus_r r in

		let _7_pd_minus_7_pc = _7_pd - _7_pc in
		let _7_pn_minus_7_pd = _7_pn - _7_pd in
		let _7_pn_minus_7_pd_minus = _7_pn_minus_7_pd - 1 in

    let r' = min _7_pn_minus_7_pd _7_pn_minus_7_pd_minus in

		let _7_pn_minus_r' = _7_pn - r' in  

    let __9_none = vecswap _7_pb _7_pn_minus_r' r' in
    let n' = _7_pb - _7_pa in

    let __10_none = if 1 < n' then (*sorting*) quickSort _7_a n' else () in
    let n'' = _7_pd - _7_pc in
		let _7_pn_minus_n'' = _7_pn - n'' in
    let __11_none = if 1 < n' then (*sorting*) quickSort _7_pn_minus_n'' n'' else () in ()
	in	

  (*let sorting _3_start _11_n = if _11_n < 7 then insertSort _3_start _11_n else quickSort _3_start _11_n
  in*) 
  (*sorting _1_start _1_n*) 
  quickSort _1_start _1_n
(* withtype {start:nat}{n:nat | start+n <= size }
         'a vect(size) * int(start) * int(n) * ('a * 'a -> order) -> unit *)
in 
let qs _1_vec =
  sortRange _1_vec 0 (Array.length _1_vec) 
(*withtype {size:nat} 'a vect(size) -> unit*)
in 

(* sorted checks if a list is well-sorted *)
let
sorted _2_arr =
  let len = Array.length _2_arr in
  let rec s v k =
		let k_plus = k + 1 in
    let v' = Array.get _2_arr k  in
			if v' < v then false else if k_plus = len then true else s v' k_plus
	in
		if len < 2 then true else 
			let get_0 = Array.get _2_arr 0 in
			s get_0 1 
in 
let vec = [|7;5;3;1;8;6;4;2|] 
in 
	sortRange vec 0 7
;; 



(* (*
 * This is an example showing that array bounds checking
 * is not needed for doing quicksort on an array.
 * The code is copied from SML/NJ lib with some modification.
 *)

(* 16 type annotations, which occupy about 40 lines *)

type order = LESS | EQUAL | GREATER
;;

let min m n = if le_int m n then m else n
withtype {m:int} int(m) -> {n:int} int(n) -> int(min(m, n))
;;

let{size:nat}
sortRange(arr, start, n, cmp) =
  let item i = arr..(i) withtype {i:nat | i < size } int(i) -> 'a in
  let swap (i,j) =
    let tmp = item i in arr..(i) <- item j; arr..(j) <- tmp
  withtype {i:nat}{j:nat | i < size /\ j < size } int(i) * int(j) -> unit in
  let rec vecswap (i,j,n) = if eq_int n 0 then () else begin swap(i,j); vecswap(i+1,j+1,n-1) end
  withtype {i:nat}{j:nat}{n:nat | i+n <= size /\ j+n <= size } int(i) * int(j) * int(n) -> unit in

  (* insertSort is called if there are less than 8 elements to be sorted *)
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

  (* calculate the median of three *)
  let med3(a,b,c) =
    let a' = item a and b' = item b and c' = item c in
      match cmp(a', b'), cmp(b', c') with
        LESS, LESS -> b
      | LESS, _ ->
         begin match cmp(a', c') with LESS -> c | _ -> a end
      | _, GREATER -> b
      | _ -> begin match cmp(a', c') with LESS -> a | _ -> c end
    withtype {a:nat}{b:nat}{c:nat | a < size /\  b < size /\ c < size}
             int(a) * int(b) * int(c) -> [n:nat | n < size ] int(n) in

  (* generate the pivot for splitting the elements *)

  let getPivot (a,n) =
    if le_int n 7 then a + n / 2
    else let p1 = a and pm = a + n / 2 and pn = a + n - 1 in
      if le_int n 40 then med3(p1,pm,pn)
      else let d = n / 8 in
           let p1 = med3(p1,p1+d,p1+2*d) in
	   let pm = med3(pm-d,pm,pm+d) in
	   let pn = med3(pn-2*d,pn-d,pn) in med3(p1,pm,pn)
  withtype {a:nat}{n:nat | 1 < n /\ a + n <= size}
           int(a) * int(n) -> [p:nat | p < size] int(p) in
		    
  let rec quickSort ((a, n) as arg) =
    let rec bottom(limit, ((pa, pb) as arg)) = (* this was defined as a higher order function in SML/NJ library *)
      if gt_int pb limit then arg
      else match cmp(item pb,item a) with
             GREATER -> arg
           | LESS -> bottom(limit, (pa, pb+1))
           | _ -> begin swap arg; bottom(limit, (pa+1,pb+1)) end
    withtype {l:nat}{ppa:nat}{ppb:nat | l < size /\ ppa <= ppb <= l+1 }
             int(l) * (int(ppa) * int(ppb)) ->
             [pa:nat][pb:nat | ppa <= pa <= pb <= l+1] (int(pa) * int(pb))

    and top(limit, ((pc, pd) as arg)) = (* this was defined as a higher order function in SML/NJ library *)
      if gt_int limit pc then arg
      else match cmp(item pc,item a) with
             LESS -> arg
           | GREATER -> top(limit, (pc-1,pd))
           | _ -> begin swap arg; top(limit, (pc-1,pd-1)) end
    withtype {l:nat}{ppc:nat}{ppd:nat | 0 < l <= ppc+1 /\ ppc <= ppd < size }
              int(l) * (int(ppc) * int(ppd)) ->
              [pc:nat][pd:nat | l <= pc+1 /\ pc <= pd <= ppd] (int(pc) * int(pd)) in

    let rec split (pa,pb,pc,pd) =
      let (pa,pb) = bottom(pc, (pa,pb)) in
      let (pc,pd) = top(pb, (pc,pd)) in
        if ge_int pb pc then (pa,pb,pc,pd)
        else begin swap (pb,pc); split (pa,pb+1,pc-1,pd) end
    withtype {ppa:nat}{ppb:nat}{ppc:nat}{ppd:nat | 0 < ppa <= ppb <= ppc+1 /\ ppc <= ppd < size }
             int(ppa) * int(ppb) * int(ppc) * int(ppd) ->
             [pa:nat][pb:nat][pc:nat][pd:nat | ppa <= pa <= pb <= pc+1 /\ pc <= pd <= ppd]
             (int(pa) * int(pb) * int(pc) * int(pd)) in

    let pm = getPivot arg in
    let _ = swap(a,pm) in
    let pa = a + 1 
    and pc = a + n - 1 in
    let (pa,pb,pc,pd) = split(pa,pa,pc,pc)
    and pn = a + n in

    let r = min (pa - a) (pb - pa) in
    let _ = vecswap(a, pb - r, r) in
    let r = min (pd - pc) (pn - pd - 1) in
    let _ = vecswap(pb, pn - r, r) in
    let n' = pb - pa in
    let _ = if gt_int n' 1 then sorting(a,n') else () in
    let n' = pd - pc in
    let _ = if gt_int n' 1 then sorting(pn-n',n') else () in ()
  withtype {a:nat}{n:nat | 7 <= n /\ a+n <= size } int(a) * int(n) -> unit
		
  and sorting ((_, n) as arg) = if lt_int n 7 then insertSort arg else quickSort arg
  withtype {a:nat}{n:nat |  a+n <= size } int(a) * int(n) -> unit in
  sorting (start,n)
withtype {start:nat}{n:nat | start+n <= size }
         'a vect(size) * int(start) * int(n) * ('a * 'a -> order) -> unit
;;

let qs vec =
  let cmp (i, j) =
    let res = compare i j in
      if res < 0 then LESS
      else if res > 0 then GREATER
           else EQUAL
  in sortRange(vec, 0, (vect_length vec), cmp)
withtype {size:nat} 'a vect(size) -> unit
;;

(* sorted checks if a list is well-sorted *)
let{size:nat}
sorted cmp arr =
  let len = vect_length arr in
  let rec s(v,i) =
    let v' = arr..(i) in
      match cmp(v,v') with
        GREATER -> false
      | _ -> if eq_int (i+1) len then true else s(v',i+1)
  withtype {i:nat | i < size } 'a * int(i) -> bool in
  if le_int len 1 then true else s(arr..(0),1)
withtype ('a * 'a -> order) -> 'a vect(size) -> bool
;;

let vec = [|7;5;3;1;8;6;4;2|]
;;
*)
