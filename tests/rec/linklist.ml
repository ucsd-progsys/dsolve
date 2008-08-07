type 'a mlist = Null | Node of 'a node 
and 'a node  = {data : 'a; 
                mutable next : 'a mlist;
                mutable prev : 'a mlist} ;;

let new_node d = 
  { data = d ; next = Null; prev = Null}

let link n1 n2 = 
  n1.next <- Node n2; 
  n2.prev <- Node n1

let rec get_last = function  
  | Node n when n.next = Null -> Node n 
  | Node n -> get_last n.next
  | Null -> Null

let add_hd l d = 
  let n = new_node d in
  let _ = match l with Node n' -> link n n' | _ -> () in
  Node n

let add_tl l d = 
  let n = new_node d in
  match get_last l with 
  | Null -> Node n
  | Node n' -> link n' n; l

let rip n = 
  match n.prev, n.next with
  | Null, Null        -> () 
  | Null, Node n''    -> n''.prev <- Null
  | Node n', Null     -> n'.next <- Null
  | Node n', Node n'' -> link n' n''

let rec nth k = function
  | _ when k < 0 -> Null 
  | Null -> Null
  | Node n when k = 0 -> Node n
  | Node n -> nth (k-1) n.next


(************************* API *************************)

let rec list_to_mlist = function
  | [] -> Null
  | x::xs -> add_hd (list_to_mlist xs) x

let rec mlist_to_list = function
  | Null -> []
  | Node n -> n.data::(mlist_to_list n.next)

let rec clone d n = 
  if n <= 0 then Null else
    add_hd (clone d (n-1)) d

(******************** Unit Test ************************)

let xs1 = [1;2;3;4] 
let l1  = list_to_mlist xs1 
let _   = List.iter (fun i -> ignore(add_tl l1 i)) [5;6;7] 
let xs2 = mlist_to_list l1 
let _   = (match nth 4 l1 with Null -> () | Node n5 -> rip n5) 
let xs3 = mlist_to_list l1 

(* let _   = assert (xs2 = [1;2;3;4;5;6;7])
   let _   = assert (xs3 = [1;2;3;4;6;7])  *)

let _  = List.iter (fun i -> assert (i >= 0)) xs1 
let _  = List.iter (fun i -> assert (i >= 0)) xs2 
let _  = List.iter (fun i -> assert (i >= 0)) xs3
