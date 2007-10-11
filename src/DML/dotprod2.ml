qualif LTs(x) : x < Array.length v1;;
qualif LEs(x) : x <= Array.length v1;;
qualif NEs(s) : not(x = Array.length v1);;
qualif LTs1(x) : x < Array.length v2;;
qualif LEs1(x) : x <= Array.length v2;;
qualif NEs1(s) : not(x = Array.length v2);;
qualif LT0(x) : x < 0;;
qualif LE0(x) : x <= 0;;
qualif NE0(x) : not(x = 0);;
qualif GT0(x) : 0 < x;;
qualif GE0(x) : 0 <= x;;
qualif SE3(x) : Array.length x = 3;;


let dotprod v1 v2 = 
  let sz = Array.length v1 in
  let sum = ref 0 in
	let i = ref 0 in
	let rec loop _none = 
		let deref_i = !i in
		let deref_i_plus = deref_i + 1 in
		if deref_i < sz then   
     		(	let get_v1_i = Array.get v1 deref_i in	
					let get_v2_i = Array.get v2 deref_i in
					let get_prod_i = get_v1_i * get_v2_i in
					let deref_sum = !sum in
					let get_prod_i_plus_deref_sum = get_prod_i + deref_sum in
					sum := get_prod_i_plus_deref_sum; 
					i := deref_i_plus;
					loop () ) 
		else ()
    in (loop (); !sum)
in
let v1' = [|1;2;3|] in
let v2' = [|2;3;4|] in
dotprod v1' v2'
;;



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
