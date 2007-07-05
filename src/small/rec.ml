qual P(x): 0 < x;;

? letrec f = fun n -> if n = 0 then 1 else f (n - 1) in f 2;;
