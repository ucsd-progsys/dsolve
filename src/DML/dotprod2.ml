qualifier LTs(x) = x < size v1;;
qualifier LEs(x) = x <= size v1;;
qualifier NEs(s) = not(x = size v1);;
qualifier LTs1(x) = x < size v2;;
qualifier LEs1(x) = x <= size v2;;
qualifier NEs1(s) = not(x = size v2);;
qualifier LT0(x) = x < 0;;
qualifier LE0(x) = x <= 0;;
qualifier NE0(x) = not(x = 0);;
qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;


let dotprod v1 v2 = 
	let sz = size v1 in
	let sz2 = size v2 in
		if sz2 = sz then

begin
	let sz_minus = sz - 1 in
  let sum = ref 0 in
	let i = ref 0 in
	let rec loop _none = 
		let deref_i = !i in
		let deref_i_plus = deref_i + 1 in
		if deref_i < sz then   
     		(	let get_v1_i = get v1 deref_i in	
					let get_v2_i = get v2 deref_i in
					let get_prod_i = get_v1_i * get_v2_i in
					let deref_sum = !sum in
					let get_prod_i_plus_deref_sum = get_prod_i + deref_sum in
					sum := get_prod_i_plus_deref_sum; 
					i := deref_i_plus;
					loop () ) 
		else ()
    in (loop (); !sum)
end 

		else -1
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
