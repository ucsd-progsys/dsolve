let bsearch key vec =
  let rec look lo hi =
      if true then
        let hl = hi + lo in
        let m = hl / 2  in
        let x = Junkarray.get vec m in
        let diff = key - x in
          (if diff < 0 then look lo (m - 1) 
           else if 0 < diff then look (m + 1) hi
           else if key = x then m else -1)
      else -1
  in
  let sv = Junkarray.length vec in
  let sv_minus = sv - 1 in 
    look 0 sv_minus

let driver =
  let _  = Random.init 555 in
  let sz = Random.int 10 in
  let sz_plus = sz + 2 in
  let ar = Junkarray.make sz_plus 0 in
  bsearch 5 ar
