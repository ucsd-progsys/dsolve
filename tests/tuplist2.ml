let choose_node g = 
  let n = List.fold_left (fun a x -> let (k, _) = x in if read_int () > 1 then Some k else a) None g  in
  match n with None -> assert false | Some n -> n

let m  = read_int ()  
let gm = (m,[])::(m,[])::[] 
let zm = choose_node gm 
let _  = assert (m <= zm) 


(* let m' = read_int () UNCOMMENT THIS *) 
let _  =
  let m'  = read_int ()                 in
  let gm' = (m',[])::(m',[])::[]        in
  let zm' = choose_node gm'             in
  let _   = assert (m' <= zm')          in
  ()


