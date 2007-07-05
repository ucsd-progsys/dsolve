qual P(x): 0 < x;;

? letrec f = fun x -> if x = 0 then 1 else f (x - 1) in f 2;;
