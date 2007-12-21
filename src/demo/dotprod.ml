qualif Positive_v1_length(v) : Array.length v >= 1
qualif Length_geq_v1_length(v) : Array.length v >= Array.length v1
qualif NonNegative(v) : v >= 0
qualif Positive(v) : v >= 1
qualif Geq_v1_length(v) : v >= Array.length v1
qualif Leq_v1_length(v) : v <= Array.length v1

let dotprod v1 v2 = 
	let rec loop n sum i =
		if i = n then sum else 
      let v1_i = Array.get v1 i in
      let v2_i = Array.get v2 i in
      let newsum = (v1_i * v2_i) + sum in
        loop n newsum (i + 1)
	in loop (Array.length v1) 0 0 

let res =
  Random.init 555;
  let sz = Random.int 40 in
  let sz_plus = sz + 1 in
  let v1 = Array.make sz_plus 1 in
  let v2 = Array.make sz_plus 1 in
    dotprod v1 v2
