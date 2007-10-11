qualif LTs(x) : x < Array.length v1;;
qualif LEs(x) : x <= Array.length v1;;
qualif NEs(s) : -.(x = Array.length v1);;
qualif LTs1(x) : x < Array.length v2;;
qualif LEs1(x) : x <= Array.length v2;;
qualif NEs1(s) : -.(x = Array.length v2);;
qualif LT0(x) : x < 0;;
qualif LE0(x) : x <= 0;;
qualif NE0(x) : -.(x = 0);;
qualif GT0(x) : 0 < x;;
qualif GE0(x) : 0 <= x;;
qualif LTn(x) : x < n;;
qualif LEn(x) : x <= n;;
qualif NEn(x) : -.(x = n);;
qualif GTn(x) : n < x;;
qualif GEn(x) : n <= x;;
qualif LT3(x) : x < 3;;
qualif LE3(x) : x <= 3;;
qualif NE3(x) : -.(x = 3);;
qualif EQ3(x) : x = 3;;
qualif SEQ3(x) : Array.length x = 3;;

(* most of these are -. needed *)


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
    let sz = Array.length v1 in
    loop sz 0 0 
in  
let v1' = Array.make 3 1 in
let v2' = Array.make 3 1 in
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
