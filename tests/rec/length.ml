let rec len x =
  match x with
  | [] -> 0
  | s::ss -> 1 + len ss

let rec append xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> x::(append xs' ys)

let rec rev x =
  match x with
  | [] -> []
  | x::xs -> append (rev xs) [x]
               (* TODO: define @ in builtins? *)

let rev2 xs =
  let rec _r xs ys = 
    match xs with 
    | [] -> ys
    | x::xs' -> _r xs' (x::ys) in
  _r xs []

let rec partition f x =
  match x with
  | [] -> 
      ([],[])
  | x::xs -> 
      let (ys,zs) = partition f xs in
      if f x then (x::ys,zs) else (ys,x::zs)

                                    (* xs in outer and inner scope is probably
                                     * causing a name collision in qualifier
                                     * instantiation -- fix eventually *)
let partition2 f ixs =
  let rec _p xs ys zs =
    match xs with 
    | [] -> 
        (fun x -> x) (ys,zs)
    | x::xs' ->
        if f x then _p ((fun x -> x) xs') (x::ys) zs else _p xs' ys (x::zs) in
  _p ixs [] []

let rec unzip x =
  match x with
  | [] -> 
      ([],[])
  | p::xs ->  (* TODO: desugar pattern matches in normalizer -- eg tuples in a cons need to become assigns in case expr *)
      let (y, z) = p in
      let (ys,zs) = unzip xs in
      (y::ys,z::zs)

let check xs ys zs = 
  let _ = assert (len (append xs ys) = len xs + len ys) in
  let _ = assert (len (rev xs) = len xs) in
  let _ = assert (len (rev2 xs) = len xs) in
  let _ = 
    let (ys,zs) = partition ((>) 0) xs in
    assert (len ys + len zs = len xs) in
  let _ = 
    let (ys,zs) = partition2 ((>) 0) xs in
    assert (len ys + len zs = len xs) in
  let _ = 
    let (xs',ys') = unzip zs in
    assert (len xs' = len zs && len ys' = len zs) in
  ()
