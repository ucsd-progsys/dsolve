(* DSOLVE -dontminemlq *)

let rec heapify size data i =
  let left  = 2 * i in
  let right = 2 * i in
  let left  = left + 1 in
  let right = right + 2 in
  let largest = 
    if left < size then
      let gl = Junkarray2.get data left in
      let gi = Junkarray2.get data i in
      if gl > gi then left else i 
    else i
  in
  let largest =
    if right < size then
      let gr = Junkarray2.get data right in
      let gx = Junkarray2.get data largest in 
      if gr > gx then right else largest
    else largest
  in
  if largest > i then
    let temp  = Junkarray2.get data i in
    let temp2 = Junkarray2.get data largest in
    let _     = Junkarray2.set data i temp2 in
    let _     = Junkarray2.set data largest temp in
    heapify size data largest
  else ()

let buildheap size data =
  let rec loop i =
    if i >= 0 then
      let _ = heapify size data i in 
      loop (i - 1) 
    else ()
  in loop ((size / 2) - 1) 

let heapsort maxx size data =
  let  _ = buildheap size data in
  let rec loop i =
    if i >= 1 then
      let temp = Junkarray2.get data i in
      let gd0  = Junkarray2.get data 0 in
      let _    = Junkarray2.set data i gd0 in
      let _    = Junkarray2.set data 0 temp in
      let _    = heapify i data 0 in
      loop (i - 1) 
    else ()
  in loop (maxx - 1)

let print_array	data i j =
  let rec loop k =
    if k < j then
      let sdk = Junkarray2.get data k in
      let _   = print_float sdk in
      loop (k+1) 
    else ()
  in
  loop i

let driver =
  let maxx = 16 in
  let data = Junkarray2.make maxx 0.0 in
  for i = 0 to maxx - 1 do 
    let isq  = i * i in
    let i_16 = 16 * i in 
    let diff = i_16 - isq in
    Junkarray2.set data i (float_of_int diff) 
  done; 
  print_string "before: "; 
  print_array data 0 maxx;
  heapsort maxx 0 data; 
  print_string "\nafter: "; 
  print_array data 0 maxx;
  print_string "\n"
