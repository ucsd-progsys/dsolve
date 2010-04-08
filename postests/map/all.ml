(* DSOLVE -dontgenmlq *)

(* Adapted from OCaml's Map module by Xavier Leroy
 * http://camlcvs.inria.fr/cgi-bin/cvsweb/~checkout~/ocaml/stdlib/map.ml?rev=1.15.6.2;content-type=text%2Fplain
 *)

type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let height t =
  match t with
  | Empty -> 0
  | Node (_,_,_,_,h) -> h

let rec set_of t =
  match t with
  | Empty -> Myaset.empty
  | Node (d, _, l, r, _) -> 
      Myaset.cup (Myaset.sng d) (Myaset.cup (set_of l) (set_of r))

let create x d l r =
  let hl = height l in
  let hr = height r in
    Node (x, d, l, r, if hl >= hr then hl + 1 else hr + 1)

let bal x d l r =
  let hl = height l in
  let hr = height r in
    if hr > hl + 2 then
      match r with
          Empty -> assert (0 = 1); assert false (* invalid_arg "Map.bal" *)
        | Node(rv, rd, rl, rr, h) ->
            if height rr >= height rl then
              create rv rd (create x d l rl) rr
            else begin
              match rl with
                  Empty -> assert (0 = 1); assert false (* invalid_arg "Map.bal" *)
                | Node(rlv, rld, rll, rlr, h) ->
                    create rlv rld (create x d l rll) (create rv rd rlr rr)
            end
    else if hl > hr + 2 then
      match l with
          Empty -> assert (0 = 1); assert false (* invalid_arg "Map.bal" *)
        | Node (lv, ld, ll, lr, h) ->
            if height ll >= height lr then
              create lv ld ll (create x d lr r)
            else begin
              match lr with
                  Empty -> assert (0 = 1); assert false (* invalid_arg "Map.bal" *)
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
        (* let c = Ord.compare x v in *)
        if x = v (* c = 0 *) then
          Node(x, data, l, r, h) 
        else if x < v (* c < 0 *) then
          bal v d (add x data l) r
        else
          bal v d l (add x data r)

let show x = x

let rec remove_min_binding t = match t with
    Empty -> assert (0 = 1); assert false
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
      | Node(r, _, rl, rr, h2) ->
          let (x, d, t2') = remove_min_binding t2 in
            bal x d t1 t2'
            
let rec nfind x t = match t with
    Empty ->
      ()
  | Node(d, _, l, r, _) ->
      let xx = nfind x l in
      let yy = nfind x r in ()

let rec remove x t = match t with
    Empty ->
      Empty
  | Node(v, d, l, r, h) ->
      if x = v then
        let xx = nfind x l in
        let xx = nfind x r in
          merge x l r
      else if x < v then
        let xx = nfind x r in
        bal v d (remove x l) r
      else
        let xx = nfind x l in
        bal v d l (remove x r)

let rec find t x = match t with
    Empty ->
      let _ = assert (1=0) in assert false 
  | Node(d, _, l, r, _) ->
      if x = d then d else
        if x < d then 
          let xx = nfind x r in
          find l x
          (*if set_of r != Myaset.empty then
            let xx = Myaset.xtr (set_of r) in
            find l x
          else find l x*)
        else
          let xx = nfind x l in
          find r x
          (*if set_of l != Myaset.empty then
            let xx = Myaset.xtr (set_of l) in
            find r x
          else find r x*)
          
