(* Balanced Binary Search Tree [from Ocaml's Map library] *)

type 'a t =
  | Empty
  | Node of 'a * int * 'a t * 'a t * int

let height t =
  match t with
  | Empty -> 0
  | Node (_,_,_,_,h) -> h

let empty = Empty

let create x d l r =
  let hl = height l in
  let hr = height r in
    Node (x, d, l, r, if hl >= hr then hl + 1 else hr + 1)

let bal x d l r =
  let hl = height l in
  let hr = height r in
    if hr > hl + 2 then
      match r with
          Empty -> assert false 
        | Node(rv, rd, rl, rr, h) ->
            if height rr >= height rl then
              create rv rd (create x d l rl) rr
            else begin
              match rl with
                  Empty -> assert false
                | Node(rlv, rld, rll, rlr, h) ->
                    create rlv rld (create x d l rll) (create rv rd rlr rr)
            end
    else if hl > hr + 2 then
      match l with
          Empty -> assert false 
        | Node (lv, ld, ll, lr, h) ->  
            if height ll >= height lr then
              create lv ld ll (create x d lr r)
            else begin
              match lr with
                  Empty -> assert false 
                | Node(lrv, lrd, lrl, lrr, h) ->
                    create lrv lrd (create lv ld ll lrl) (create x d lrr r)
            end
    else
      Node(x, d, l, r, if hl >= hr then hl + 1 else hr + 1)

let rec add x data t =
  match t with
      Empty ->
        Node(x, data, Empty, Empty, 1)
    | Node(v, d, l, r, h) ->
        if x = v then
          Node(x, data, l, r, h)
        else if x < v then
          bal v d (add x data l) r
        else
          bal v d l (add x data r)

let rec remove_min_binding t = match t with
    Empty -> assert false
  | Node(x, d, l, r, h) ->
      match l with
      | Empty -> (x, d, r)
      | Node(_, _, ll, lr, h') ->
        let (x', d', l') = remove_min_binding l in
          (x', d', bal x d l' r)

let merge m t1 t2 =
  match t1 with
  | Empty -> t2
  | Node(_, _, ll, lr, h1) -> 
      match t2 with
      | Empty -> t1
      | Node(_, _, rl, rr, h2) ->
          let (x, d, t2') = remove_min_binding t2 in
            bal x d t1 t2'

let rec remove x t = 
  match t with
  | Empty ->
      Empty
  | Node(v, d, l, r, h) ->
      if x = v then
        merge x l r
      else if x < v then
        bal v d (remove x l) r
      else
        bal v d l (remove x r)
       
let rec iter f t = 
  match t with
  | Empty                -> ()
  | Node (k, d, l, r, _) -> f k d; iter f l; iter f r

(****************************** checker ******************************)

let rec check_bst = function 
  | Empty                -> ()
  | Node (v, _, l, r, _) -> iter (fun k d -> assert (k < v)) l; 
                            iter (fun k d -> assert (v > k)) r;
                            check_bst l; check_bst r


(************************************************************************)
(********* maximal, non-deterministic client i.e. test harness **********)
(************************************************************************)

let rec make_random t = 
  if read_int () = 0 then 
    let k = read_int () in
    let d = read_int () in
    make_random (add k d t) 
  else if read_int () = 0 then 
    let k = read_int () in
    make_random (remove k t) 
  else
    t 

let _ = check_bst (make_random empty) 

