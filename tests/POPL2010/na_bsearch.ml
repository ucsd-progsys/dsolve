let bsearch key vec =
  let rec look lo hi =
    let hi_minus = hi - 1 in
      if lo < hi_minus  then
        let hl = hi + lo in
        let m = hl / 2  in
        let x = Junkarray2.get vec m in
        let diff = key - x in
        let m_plus = m + 1 in
        let m_minus = m - 1 in
          (if diff < 0 then look lo m_minus
           else if 0 < diff then look m_plus hi
           else if key = x then m else -1)
      else -1
  in
  let sv = Junkarray2.length vec in
  let sv_minus = sv - 1 in 
    look 0 sv_minus

let driver =
  let _  = Random.init 555 in
  let sz = Random.int 10 in
  let sz_plus = sz + 2 in
  let ar = Junkarray2.make sz_plus 0 in
  bsearch 5 ar
