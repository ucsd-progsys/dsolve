pred U(x): x < alen;;
pred L(x): 0 <= x;;

let qsort = fun a ->
  let b = New in
  let recsort = fun l -> fun h ->
    if h <= l then
      b
    else
      
