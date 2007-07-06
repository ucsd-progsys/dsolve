qual DEAD(x): not(true);;

? let y = 3 in
let x = 3 in
  if y = x then
    let k = y + 1 in
      if x < k then
        (fun w -> w) 1
      else
        (fun z -> z) 0
  else
    (fun n -> n) -1;;

# This is highly suspicious doping; we need to pass through a function to
# make sure that we get the implications out of the frame.
