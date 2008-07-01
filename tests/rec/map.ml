type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let height t =
  match t with
  | Empty -> 0
  | Node (_,_,_,_,h) -> h

let pheight t =
  match t with
  | Empty -> 0
  | Node (_,_,l,r,_) -> if height l >= height r then height l + 1 else height r + 1

let show x = x

let create x d l r =
  let hl = height l in
  let hr = height r in
  let h = if hl >= hr then hl + 1 else hr + 1 in
  Node (x, d, l, r, h)

let bal x d l r =
  let hl = height l in
  let hr = height r in
    if hr > hl + 2 then
      (*match r with
          Empty -> assert false (* invalid_arg "Map.bal" *)
        | Node(rv, rd, rl, rr, _) ->
            if height rr >= height rl then
              create rv rd (create x d l rl) rr
            else begin
              match rl with
                  Empty -> assert false (* invalid_arg "Map.bal" *)
                | Node(rlv, rld, rll, rlr, _) ->
                    create rlv rld (create x d l rll) (create rv rd rlr rr)
            end*)
      assert false
    else if hl > hr + 2 then
      let _ = show hr in
      let _ = show hl in
      match l with
          Empty -> assert false (* invalid_arg "Map.bal" *)
        | Node (lv, ld, ll, lr, h) -> (* we know by driver that ll = lr *)
            let _ = assert (height ll <= height l - 1) in
            let _ = assert (height lr <= height l - 1) in
            let _ = assert (height ll >= height lr - 2) in
            let _ = assert (height ll <= height lr + 2) in
            (*let _ = assert (height ll >= height lr) in*)
            let _ = assert (height ll >= height r) in
            let _ = assert (height lr >= height r) in
            if height ll >= height lr then
              let _ = assert (height ll <= height lr + 2) in
              let _ = show ll in
              let _ = show lr in
              let _ = show (height ll) in
              create lv ld ll (create x d lr r)
            else begin
              match lr with
                  Empty -> assert false (* invalid_arg "Map.bal" *)
                | Node(lrv, lrd, lrl, lrr, _)->
                    (*create lrv lrd (create lv ld ll lrl) (create x d lrr r)*)
                    assert false
            end
    else
      (*Node(x, d, l, r, if hl >= hr then hl + 1 else hr + 1)*)
      assert false

let balanced t =
  match t with
  | Empty -> assert false
  | Node(_, _, l, r, h) ->
     let hl = height l in
     let hr = height r in
     let _ = assert (h = (if hl >= hr then hl + 1 else hr + 1)) in
     let _ = assert (hl <= hr + 2) in
     let _ = assert (hl >= hr - 2) in
     let _ = assert (1 = 0) in
    ()

let checker () = 
  let t0 = Empty in
  (*let t1' = create 0 0 t0 t0 in*)
  let t1 = Node(0, 0, t0, t0, 1) in
  let t2 = Node(1, 1, t0, t1, 2) in
  let t3 = Node(1, 1, t2, t0, 3) in
  let t4 = Node(0, 0, t1, t3, 4) in
  let t5 = bal 0 0 t4 t1 in
  let _ = assert (height t5 <= height t4 + 1) in
  let _ = assert (height t5 <= height t4 + 2) in
  let _ = assert (height t5 >= height t4) in
  let _ = balanced t5 in
  let _ = assert (1 = 0) in
  (*let t1 = create 0 0 t0 t0 in 
  let t2 = create 1 1 t0 t1 in
  let t3 = create 2 2 t1 t0 in*) 
  let _ = show t1 in
  (*let _ = show t2  in
  let _ = show t3 in*) 
  (* let t3 = create 2 2 t1 t0 in
  let t2 = create 1 0 t1 t0 in
  let _ = show create in *)
  ()


(*
let check x e =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  let sz1 = Node(x, e, Empty, Empty, a) in
  let sz2 = Node(x, e, sz1, sz1, b) in
  let sz3 = Node(x, e, sz2, sz2, c) in
  let sz4 = Node(x, e, sz3, sz3, d) in
  let _ = assert (show (height (show sz4)) = 4) in
  let _ = assert (height sz1 = 1) in
  let _ = assert (height sz2 = 3) in
    bal x d sz1 sz4

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

let rec checker = function
  | Empty -> ()
  | Node (v, d, l, r, h) ->
      let _ = match l with Empty -> () | Node (v',d',l',r',h') -> assert (v' <= v) in
      let _ = match r with Empty -> () | Node (v',d',l',r',h') -> assert (v' >= v) in
      let _ = checker l; checker r in ()

let test_add x d x' d' x'' d'' =
  let z = add x d Empty in
  let o = add x' d' z in
  let t = add x'' d'' o in
    checker t

let rec remove_min_binding t = match t with
    Empty -> assert false
  | Node(x, d, Empty, r, _) -> (x, d, r)
  | Node(x, d, l, r, _) ->
      let (x', d', l') = remove_min_binding l in
        (x', d', bal x d l' r)

let merge m t1 t2 =
  match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
        let (x, d, t2') = remove_min_binding t2 in
          bal x d t1 t2'

let rec remove x t = match t with
    Empty ->
      Empty
  | Node(v, d, l, r, _) ->
      if x = v then
        merge x l r
      else if x < v then
        bal v d (remove x l) r
      else
        bal v d l (remove x r)

let test_remove x d x' d' =
  let z = add x d Empty in
  let o = add x' d' z in
  let t = remove x o in
    checker t*)

(*


let empty = Empty

    let is_empty = function Empty -> true | _ -> false

(*    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          (* let c = Ord.compare x v in *)
          if x = v (*c = 0*) then d
          else find x (if x < v (* c < 0 *) then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          (* let c = Ord.compare x v in*)
          x = v (* c = 0 *) || mem x (if x < v (* c < 0 *) then l else r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

    let rec mapi f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))
 *)

    (*****************************************************************)
    (* 
    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End) *)

*)
