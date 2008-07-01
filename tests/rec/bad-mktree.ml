type tree = Empty | Node of int * tree * tree

let rec mktree l h =
  if h < l then
    Empty
  else
    let r = (l + h) / 2 in
      Node (r, mktree (r + 1) h, mktree l (r - 1))

let rec checkleft x t =
  match t with
    | Empty -> ()
    | Node (v, l, r) ->
        assert (v <= x);
        checkleft v l;
        checkright v r

and checkright x t =
  match t with
    | Empty -> ()
    | Node (v, l, r) ->
        assert (v >= x);
        checkleft v l;
        checkright v r

let checktree t =
  match t with
    | Empty -> ()
    | Node (v, l, r) ->
        checkleft v l;
        checkright v r

let t = mktree 0 (Random.int 100)

let _ = checktree t
