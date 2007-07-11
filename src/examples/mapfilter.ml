qual P(x): 0 < x;;
qual N(x): x < 0;;

? letrec mapfilter = fun f -> fun l ->
  match l with
      [] ->
	[]
    | h::t ->
	let r = mapfilter f t in
	let x = f h in
	  match x with
              [] ->
                r
            | z::e ->
                z::r
in
let pos = fun y ->
  if 0 < y then
    y::[]
  else
    []
in
  mapfilter pos 1::2::-1::[];;
