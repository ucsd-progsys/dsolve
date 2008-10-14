let m0 = Mymap.make 0 0
let _  = m0
let _  = Mymap.itert m0 (fun qq js -> assert (qq >= 0))
(*
let c  = (fun x -> x + 1) 0 
let n2 = fresh n1
let n3 = fresh n2
let _  = show n0
let _  = show n1
let _  = show n2
let _  = assert (n1 <= n2)
let m1 = Mymap2.set m0 n2 n2
let m2 = Mymap2.set m1 n3 n3
let _  = Mymap2.iter m2 (fun i js -> assert (i >= n0))
*)
