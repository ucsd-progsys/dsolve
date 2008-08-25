type 'a node  = {data: 'a;
                 next: 'a node option}

let rec list_to_mlist = function
  | []      -> assert false
  | x :: [] -> {data = x; next = None}
  | x :: xs -> {data = x; next = Some (list_to_mlist xs)}

let rec mlist_to_list ml =
  ml.data :: (match ml.next with None -> [] | Some mln -> mlist_to_list mln)

let rec nth n ml =
  if n = 0 then
    ml.data
  else
    match ml.next with
      | None     -> assert false
      | Some mln -> nth (n - 1) mln

let x1 = list_to_mlist [1;2;3]
let x  = nth 2 x1
let _  = assert (x = 0)

let x2 = mlist_to_list x1
let y  = List.nth x2 2
let _  = assert (y < 0)
