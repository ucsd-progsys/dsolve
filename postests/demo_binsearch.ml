let show x = ()

let bs a v =
  let rec look lo hi =
    if lo >= hi-1 then -1 else
      let m = (hi + lo)/2 in
      let x = Array.get a m in
      if v < x then look lo (m-1) else
       if v > x then look (m+1) hi else
        m in
  look 0 (Array.length a - 1)

