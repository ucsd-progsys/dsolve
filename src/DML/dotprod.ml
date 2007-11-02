let dotprod v1 v2 = 
		let rec loop n sum i =
			if i = n then sum else 
        let get_v1_i = Array.get v1 i in  
        let get_v2_i = Array.get v2 i in
        let get_prod_i = get_v1_i * get_v2_i in
        let get_prod_i_plus_sum = get_prod_i + sum in
        let i_plus = i + 1 in
        loop n get_prod_i_plus_sum i_plus
		in
    let sz_1 = Array.length v1 in
    loop sz_1 0 0 
in  
let _none = Random.init 555 in
let sz = Random.int 40 in
let sz_plus = sz + 1 in
let v1' = Array.make sz_plus 1 in
let v2' = Array.make sz_plus 1 in
dotprod v1' v2'
;;

(*
let{n:nat} dotprod v1 v2 = begin
    loop (vect_length v1) 0 0
      where rec loop n sum i =
        if eq_int i n then sum else loop n (sum + (v1..(i) * v2..(i) : int)) (i+1)
    withtype int(n) -> int -> {i:nat | i <= n} int (i) -> int
end withtype int vect(n) -> int vect(n) -> int
;;

*)
