let rec len xs = match xs with
  | [] -> 0
  | x::xs -> 1 + len xs

let rec zip xs ys = 
  match xs, ys with
  | [], []       -> []
  | x::xs, y::ys -> (x,y)::(zip xs ys)
  | [] ,_::_     -> assert (0=1); assert false
  | _::_, []     -> assert (0=1); assert false

let rec clone x n =
  let _ = assert (n >= 0) in
  if n = 0 then [] else x::(clone x (n-1))
 
let rec combinations xss = match xss with 
  | [] ->
      [[]]
  | xs :: xss ->
      let yss  = combinations xss in
      let xyss = List.map (fun x -> 
                   List.map (fun ys -> 
                     x :: ys
                   ) yss
                 ) xs in
   (* let xyss = [ x::ys | for x in xs, ys in yss ] in *)
      List.flatten xyss

let assignments xs ys = 
  let n   = len xs in
  let yss = clone ys n in
  let zss = combinations yss in
  List.map (zip xs) zss

let a1 = assignments ["x";"y"] [1.1; 2.2; 3.3; 4.4] 

let a2 = assignments ["a"; "b"; "c"] [10; 20; 30; 40; 50]

