type t = Node of int
type tt = Pode of (int * int) * (int * int)

let printer z = 
  match z with Pode (z,z') -> 
    let (x,y) = z in
    let (x',y') = z' in
    x + y + x' + y'

let make x = 
  match x with None -> None | Some x -> Some (Node x)

let check x = 
  match x with 
  | Some y -> (match y with Node z -> assert (z>0)) 
  (* UNCOMMENT TO SEE CRASH:  
  | Some (Node x) -> assert (x > 0) *) 
  | Some (Node x) -> assert (x > 0)
  | _ -> ()

let rec go z = 
  let x = read_int () in
  let y = if x > 0 then Some x else None in
  check (make y); go z

let _ = go 12
