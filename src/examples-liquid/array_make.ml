let asize = Random.int 10 in
let asize = asize + 1 in
let asize = (fun x -> x) asize in
let a = Array.make asize 0 in
let a = (fun x -> x) a in
Array.make 5 a;;
