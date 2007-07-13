qual NM(x): x = n + m;;

? letrec append = fun n -> fun m ->
  if n = 0 then
    m
  else
    let k = n - 1 in
    let z = append k m in
      z + 1
in
  append 1 2;;
