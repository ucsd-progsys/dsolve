qualifier LTs(x) = x < Array.length v1;;
qualifier LEs(x) = x <= Array.length v1;;
qualifier NEs(s) = not(x = Array.length v1);;
qualifier LTs1(x) = x < Array.length v2;;
qualifier LEs1(x) = x <= Array.length v2;;
qualifier NEs1(s) = not(x = Array.length v2);;
qualifier LT0(x) = x < 0;;
qualifier LE0(x) = x <= 0;;
qualifier NE0(x) = not(x = 0);;
qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier LTn(x) = x < n;;
qualifier LEn(x) = x <= n;;
qualifier NEn(x) = not(x = n);;
qualifier GTn(x) = n < x;;
qualifier GEn(x) = n <= x;;
qualifier LT3(x) = x < 3;;
qualifier LE3(x) = x <= 3;;
qualifier NE3(x) = not(x = 3);;
qualifier EQ3(x) = x = 3;;


let dotprod v1 v2 = 
begin
		(* we can't infer that Array.length v2 >= Array.length v1.. so this has to be added *)
		let sz2 = Array.length v2 in
		let sz = Array.length v1 in
			if sz2 = sz then
begin
		let sz_minus = sz - 1 in
		let rec loop n sum i =
			let get_v1_i = Array.get v1 i in
			let get_v2_i = Array.get v2 i in
			let get_prod_i = get_v1_i * get_v2_i in
			let get_prod_i_plus_sum = get_prod_i + sum in
			let i_plus = i + 1 in 
			if i = n then sum else loop n get_prod_i_plus_sum i_plus
		in
    loop sz_minus 0 0  (* change sz_minus to sz to cause the array bounds check to fail *)
end
		else -1
end in  
let v1' = [|1;2;3|] in
let v2' = [|1;2;3|] in
dotprod v1' v2'
;;

(* there's a "bug" in the below code (possibly intentional), the array index actually goes out of bounds by one (in the loop n 0 0). our system detects this and won't compile, which is actually a little maddening *)


(*
let{n:nat} dotprod v1 v2 = begin
    loop (vect_length v1) 0 0
      where rec loop n sum i =
        if eq_int i n then sum else loop n (sum + (v1..(i) * v2..(i) : int)) (i+1)
    withtype int(n) -> int -> {i:nat | i <= n} int (i) -> int
end withtype int vect(n) -> int vect(n) -> int
;;

*)
