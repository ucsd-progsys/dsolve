type 'a opt =
    Nome of 'a
  | Sone

let maybe x =
  match x with
      Nome a -> (fun x -> x) 1
    | Sone -> (fun x -> x) 0

let check x =
  maybe x
