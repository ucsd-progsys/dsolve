qual NNEG(x): 0 <= x;;
qual P(x): 0 < x;;

? let app = fun f -> fun x -> f x in let inc = fun n -> n + 1 in app inc 0;;
