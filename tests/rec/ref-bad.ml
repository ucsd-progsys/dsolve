let xs1 = [1;2;3;4;5;6;7]
let _ = List.iter (fun i -> assert (i >= 0)) xs1
let x = ref 10
let y = !x
let _ = x := 100
(* let _ = x := -10 *)
let _ = assert (y >= 0)
