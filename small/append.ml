qualif NM(x): x = n + m;;
qualif T(x): x = 3;;

let rec append n m =
  if n = 0 then
    m
  else
    let k = n - 1 in
    let z = append k m in
      z + 1
in append 1 2;;
