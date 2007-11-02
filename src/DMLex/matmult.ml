let sub2 data i j = 
  let gi = Array.get data i in
    Array.get gi j in
(*withtype {m:nat, n:nat, i:nat, j:nat | i < m, j < n} <> =>
         ('a array(n)) array(m) * int(i) * int(j) -> 'a*)

let update2 data i j x = 
  let gi = Array.get data i in
    Array.set gi j x in
(*withtype {m:nat, n:nat, i:nat, j:nat | i < m, j < n} <> =>
         ('a array(n)) array(m) * int(i) * int(j) * 'a -> unit*)

let matmul a b =
  let p = Array.length a in
  let in_q = Array.get a 0 in
  let in_r = Array.get b 0 in
  let q = Array.length in_q in
  let r = Array.length in_r in
  
  let tmp_arr = Array.make r 0 in
  let cdata = Array.make p tmp_arr in
  let rec fill_arr i iq arr = 
      let len = Array.length arr in
      if i < len then 
        let new_array = Array.make iq 0 in
        let i' = i + 1 in
        Array.set arr i new_array; fill_arr i' iq arr
      else ()
  in
  let _none = fill_arr 0 r cdata in 
      
	let rec loop1 i =
	  if i < p then
		  let rec loop2 j =
			  if j < r then
			   let rec loop3 k sum = 
			     if k < q then
             let k' = k + 1 in 
             let saik = sub2 a i k in
             let sbkj = sub2 b k j in
             let sum_p = sum + saik in
             let sum_p = sum_p + sbkj in
			       loop3 k' sum_p  
				   else sum
				(*withtype {k:nat} <max(q-k,0)> => int(k) * float -> float*)
          in let l3 = loop3 0 0 in
			    let _none = update2 cdata i j l3  in
        let j' = j + 1 in
				loop2 j'
			else () in
		    (*withtype {j:nat} <max(r-j,0)> => int(j) -> unit*)
		  let _none = loop2 0 in
      let i' = i + 1 in
		  loop1 i'
    else () in
        (*withtype {i:nat} <max(p-i,0)> => int(i) -> unit*)
	loop1 0; cdata 
in 
let _none = Random.self_init () in
let p = Random.int 10 in
let p = p + 1 in
let q = Random.int 10 in
let q = q + 1 in
let r = Random.int 10 in
let r = r + 1 in
let fill _none = Random.int 100 in
let tmp = Array.make q 0 in
let tmp2 = Array.make r 0 in
let av = Array.make p tmp in
let bv = Array.make q tmp2 in
let fillar arr =
  let len = Array.length arr in 
  let rec fill_rec i =
    let i' = i + 1 in
    let r = fill () in
    if i < len then (Array.set arr i r; fill_rec i')   
    else () 
  in fill_rec 0 in
let fillbr arr ip =
  let len = Array.length arr in
  let rec fill_rec i =
    let i' = i + 1 in
    let r = Array.make ip 0 in 
    let _none = fillar r in
    if i < len then (Array.set arr i r; fill_rec i')
    else ()
  in fill_rec 0 in
fillbr av q; fillbr bv r; matmul av bv;;


(*withtype {p:nat,q:nat,r:nat} <> =>
         float matrix(p,q) * float matrix(q,r) -> float matrix(p,r)*)


(*fun matmul (a, b) =
    let
	val Matrix (p, q, adata) = a
	val Matrix (_, r, bdata) = b
	val cdata = alloc (p, alloc (r, 0.0))

	fun init (i) =
            if i < p then
	       let
		   val _ = update (cdata, i, alloc (r, 0.0))
	       in
		   init (i+1)
	       end
	    else ()
	withtype {i:pos} <max(p-i, 0)> => int(i) -> unit

	val _ = init (1)

	fun loop1 (i) =
	    if i < p then
		let
		    fun loop2 (j) =
			if j < r then
			    let
				fun loop3 (k, sum) =
				    if k < q then
					loop3 (k+1, sum +. sub2 (adata, i, k) *. sub2 (bdata, k, j))
				    else sum
				withtype {k:nat} <max(q-k,0)> => int(k) * float -> float
				val _ = update2 (cdata, i, j, loop3 (0, 0.0))
			    in
				loop2 (j+1)
			    end
			else ()
		    withtype {j:nat} <max(r-j,0)> => int(j) -> unit
		    val _ = loop2 (0)
		in
		    loop1 (i+1)
		end
	    else ()
        withtype {i:nat} <max(p-i,0)> => int(i) -> unit
	val _ = loop1 (0)
    in
	Matrix (p, r, cdata)
    end
withtype {p:nat,q:nat,r:nat} <> =>
         float matrix(p,q) * float matrix(q,r) -> float matrix(p,r)*)
