let rec len = function 
  | [] -> 0
  | x::xs -> 1 + len xs

let rec append xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> x::(append xs' ys)

let rec rev = function
  | [] -> []
  | x::xs -> (rev xs)@[x]

let rev2 xs =
  let rec _r xs ys = 
    match xs with 
    | [] -> ys
    | x::xs' -> _r xs' (x::ys) in
  _r xs []

let rec partition f = function
  | [] -> 
      ([],[])
  | x::xs -> 
      let (ys,zs) = partition f xs in
      if f x then (x::ys,zs) else (ys,x::zs)

let partition f xs =
  let rec _p xs ys zs =
    match xs with 
    | [] -> 
        (ys,zs)
    | x::xs' ->
        if f x then _p xs' (x::ys) zs else _p xs' ys (x::zs) in
  _p xs [] []

let rec unzip = function
  | [] -> 
      ([],[])
  | (y,z)::xs -> 
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
    let (xs',ys') = unzip zs in
    assert (len xs' = len zs && len ys' = len zs) in
  ()
