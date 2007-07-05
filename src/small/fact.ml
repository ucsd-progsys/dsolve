qual P(x): 0 < x;;
qual N(x): 0 <= x;;

? letrec f = fun n -> if n = 0 then 1 else let k = f (n - 1) in n + k in f 3;;
