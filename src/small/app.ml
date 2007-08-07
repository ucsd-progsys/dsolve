qualifier NNEG(x) = 0 <= x;;
qualifier P(x) = 0 < x;;

let app f x = f x in let inc n = n + 1 in app inc 0;;
