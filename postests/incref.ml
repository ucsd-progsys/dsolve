(* This works *)
let (<~) = (:=)
let w = ref 0
let _ = w <~ !w + 1
let _ = assert (!w >= 0)

let nonnull = function 
  | x::xs -> true 
  | []    -> (not (true))

(* This fails 
let y = ref 0
let _ = y := !y
let _ = assert (!y >= 0)
*)


let rec ffor i j f = 
  if i < j then begin
    f i;
    ffor (i+1) j f
  end

let min_index a =
  let min = ref 0 in
  ffor 0 (Array.length a) begin fun i ->
    if a.(i) < a.(!min) then 
      min <~ i
  end;
  !min

