pred DEAD(x): not(true);;

? if y = x then
    let k = y + 1 in
      if x < k then
        1
      else
        0
  else
    -1;;
