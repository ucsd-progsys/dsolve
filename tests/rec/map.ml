type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let height = function
  | Empty -> 0
  | Node (_,_,_,_,h) -> h

let create x d l r =
  let hl = height l and hr = height r in
    Node(x, d, l, r, if hl >= hr then hl + 1 else hr + 1)

let bal x d l r =
  let hl = height l in
  let hr = height r in
    if hl > hr + 2 then
      match r with
          Empty -> assert false (* invalid_arg "Map.bal" *)
        | Node(rv, rd, rl, rr, _) ->
            if height rr >= height rl then
              create rv rd (create x d l rl) rr
            else begin
              match rl with
                  Empty -> assert false (* invalid_arg "Map.bal" *)
                | Node(rlv, rld, rll, rlr, _) ->
                    create rlv rld (create x d l rll) (create rv rd rlr rr)
            end
    else if hr > hl + 2 then
      match l with
          Empty -> assert false (* invalid_arg "Map.bal" *)
        | Node (lv, ld, ll, lr, _) ->
            if height ll >= height lr then
              create lv ld ll (create x d lr r)
            else begin
              match lr with
                  Empty -> assert false (* invalid_arg "Map.bal" *)
                | Node(lrv, lrd, lrl, lrr, _)->
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

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

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
