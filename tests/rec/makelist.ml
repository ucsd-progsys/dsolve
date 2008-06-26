let rec len x =
  match x with
  | [] -> 0
  | s::ss -> 1 + len ss

let rec make_list n =
  if n <= 0 then [] else
   let l = make_list (n-1) in
      n :: l

let check n =
  let _ =
    assert (n <= len (make_list n)) in
  ()
