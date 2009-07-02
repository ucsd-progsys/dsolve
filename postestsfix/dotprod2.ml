let dotprod v1 v2 = 
  let sz = Array.length v1 in
  let sum = ref 0 in
	let i = ref 0 in
	let rec loop i sum = 
		if i < sz then   
      (	let get_v1_i = Array.get v1 i in	
				let get_v2_i = Array.get v2 i in
				let get_prod_i = get_v1_i * get_v2_i in
				let get_prod_i_plus_sum = get_prod_i + sum in
				loop (i+1) (get_prod_i + sum)) 
		else sum
    in loop 0

let driver =
  let _ = Random.init 555 in
  let sz = Random.int 40 in
  let sz_plus = sz + 1 in
  let v1__ = Array.make sz_plus 1 in
  let v2__ = Array.make sz_plus 1 in
    dotprod v1__ v2__ 



(*
let dotprod v1 v2 = begin
  let sum = ref 0 in
    for i = 0 to pred (vect_length v1) do
      sum := v1..(i) * v2..(i) + !sum
    done;
    !sum
end withtype {n:nat} int vect(n) -> int vect(n) -> int
;;
*)
