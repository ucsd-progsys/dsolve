let min m n = 
	if m <= n then m else n
in 
let anno = (fun x -> x) in
(*
 * This is an example showing that array bounds checking
 * is not needed for doing quicksort on an array.
 * The code is copied from SML/NJ lib with some modification.
 *)
let rec sortRange arr start n =
  let item i = Array.get arr i in
  let swap i j =
    let tmp = item i in 
    (Array.set arr i (item j); Array.set arr j tmp)
  in
  let rec vecswap i j n = 
		if n = 0 then () else (swap i j; vecswap (i+1) (j+1) (n-1)) 
  in 

  (* insertSort is called if there are less than 8 elements to be sorted *)
  (*let insertSort start n =
    let limit = start + n in
		let start_plus = start + 1 in
    let rec outer i =
			let i_plus = i + 1 in
      if limit < i_plus then ()
      else let rec inner _4_j =
             if _4_j < start_plus then outer i_plus 
             else let _4_j' = _4_j - 1 in
								  let ij' = item _4_j' in
									let ij = item _4_j in
									if ij < ij' then
										(swap _4_j _4_j'; inner _4_j') else
										outer i_plus
           in inner i
    in outer start_plus
  in*) 

  (* calculate the median of three *)
  let med3 a b c =
    let a' = item a in
		let b' = item b in 
		let c' = item c in
		let lt_ab = a' < b' in
		let lt_bc = b' < c' in
		let gt_bc = c' < b' in
		let lt_ac = a' < c' in
		if lt_ab && lt_bc then
			b else
		if lt_ab then
			if lt_ac then c else a else
		if gt_bc then b else
		if lt_ac then a else c
	in 

  (* generate the pivot for splitting the elements *)
  let getPivot a n =
    if n <= 7 then a + n/2  
    else let p1 = a in
				 let pm = a + n/2 in
				 let pn = a + n - 1 in
       	 if n <= 40 then med3 p1 pm pn else
      		 let d = n / 8 in
					 let _2d = 2 * d in
					 let p1_plus_d = p1 + d in
					 let p1_plus_2_d = p1 + _2d in
					 let pm_minus_d = pm - d in
					 let pm_plus_d = pm + d in
					 let pn_minus_2_d = pn - _2d in
					 let pn_minus_d = pn - d in
           let newp1 = med3 p1 p1_plus_d p1_plus_2_d in
	   			 let newpm = med3 pm_minus_d pm pm_plus_d in
	   			 let newpn = med3 pn_minus_2_d pn_minus_d pn in 
							med3 newp1 newpm newpn
	in	
  let rec quickSort a n =
    let rec bottom limit pa pb = 
      let _ = anno pa in
      let _ = anno pb in
			let arg = {p1 = pa; p2 = pb} in
      let _ = anno arg in
      if limit < pb then {p1 = pa; p2 = pb} else
			if item a < item pb then {p1 = pa; p2 = pb} else
				if item pb < item a then bottom limit pa (pb+1) else
						(swap pa pb; bottom limit (pa+1) (pb+1))
		in 
    let rec top limit pc pd = 
      let _ = anno pc in
      let _ = anno pd in
			let arg = {p1 = pc; p2 = pd} in
      let _ = anno arg in
      if pc < limit then arg else
      if item pc < item a then arg else
			if item a < item pc then top limit (pc-1) pd else
			(swap pc pd; top limit (pc-1) (pd-1)) 
    in 
    let rec split pa pb pc pd =
			let pp1 = bottom pc pa pb in
      let pa = pp1.p1 in
      let pb = pp1.p2 in
      let _ = anno pa in
      let _ = anno pb in
      let pp2 = top pb pc pd in
      let pc = pp2.p1 in
      let pd = pp2.p2 in
      let _ = anno pc in
      let _ = anno pd in
      if pb >= pc then {q1 = pa; q2 = pb; q3 = pc; q4 = pd}
      else 
      (swap pb pc; 
								 split pa (pb+1) (pc-1) pd) 
 	  in 

    let pm = getPivot a n in
    let pa = a + 1 in
    let pd = a + n - 1 in
    let _ = anno n in
    let _ = anno pa in
    let _ = anno pd in
    let _ = swap a pm in
    let sp = split pa pa pd pd in
    let _ = (fun x -> x) sp in
    let pa = sp.q1 in
    let pb = sp.q2 in
    let pc = sp.q3 in
    let pd = sp.q4 in

    let pn = a + n in
		
    (*let r = min (pa-a) (pb-pa) in
    let _ = vecswap a (pb-r) r in*)

    let r = min (pd-pc) (pn-pd-1) in
    let _ = (fun x -> x) pd in
    let _ = (fun x -> x) pa in
    let _ = (fun x -> x) pb in
    let _ = (fun x -> x) pc in
    let _ = (fun x -> x) r in
    (*let _ = vecswap pb start (*(pn-r)*) r in*)
    (*if (pn-r) + r <= pn then*)
      let _ = vecswap pb (pn-r) r in
    let n' = pb - pa in

    (*************)
    (*let _ = (fun x -> x) a in
    let _ = (fun x -> x) n in
    let pn = a + n in
    let n' = 0 in*)
    (*************)

    (*let _none = if 1 < n' then if n' > 7 then quickSort a n' else insertSort a n' else () in*)
    let _ = (fun y -> y) a in
    let _ = (fun y -> y) n' in
    let _ = if 1 < n' then quickSort a n'(*sortRange arr a n'*) else () in
    (*let n' = pd - pc in
      
    (************)
    let n' = 0 in
    (************) 

		let pn_minus_n' = pn - n' in
    (*let __11_none = if 1 < n'' then if n'' > 7 then quickSort pn_minus_n'' n'' else insertSort a n'' else () in ()*)
    let _ = (fun y -> y) pn_minus_n' in
    let _ = (fun y -> y) n' in
    let _ = if 1 < n' then quickSort pn_minus_n' n'(*sortRange arr pn_minus_n' n'*) else () in*) ()
	in	
  quickSort start n
  (*let sorting _3_start _11_n = if _11_n < 7 then insertSort _3_start _11_n else quickSort _3_start _11_n
  in*) 
  (*sorting start n*) 
    (*quickSort start n*)
  (*if n <= 7 then insertSort start n else quickSort start n*)
in 
(*let qs _1_vec =
  sortRange _1_vec 0 (Array.length _1_vec) *)
(*in*) 

(* sorted checks if a list is well-sorted *)
(*let
sorted arr =
  let len = Array.length arr in
  let rec s v k =
		let k_plus = k + 1 in
    let v' = Array.get arr k  in
			if v' < v then false else if k_plus = len then true else s v' k_plus
	in
		if len <= 1  then true else 
			let get_0 = Array.get arr 0 in
			s get_0 1 
in*) 
let _ = Random.self_init ()in
let p = Random.int 20 + 2 in
let x = let x : garbage = 0 in x in
let vec = Array.make p x in
	sortRange vec 0 p; vec(*sorted vec*);;
 
