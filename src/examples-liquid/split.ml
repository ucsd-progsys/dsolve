let rec quickSort arr a n =
    let item i = Array.get arr i in
    let swap i j =
      let tmp = Array.get arr i in 
      let ij = Array.get arr j in
      (Array.set arr i ij; Array.set arr j tmp)
    in
    let rec bottom limit pa pb = 
      let _ = (fun x y -> (x, y)) pa pb in
			let arg = {ft = pa; sd = pb} in
      let ia = item a in
      let pb' = pb + 1 in
      let pa' = pa + 1 in
      if limit < pb then arg else
      let ipb = item pb in
			if ia < ipb then arg else
				if ipb < ia then bottom limit pa pb' else
						(swap pa pb; bottom limit pa pb')
		in 
    let rec top limit pc pd = 
			let arg = {ft = pc; sd = pd} in
      if pc < limit then arg else
      if item pc < item a then arg else
			if item a < item pc then top limit (pc-1) pd else
			(swap pc pd; top limit (pc-1) pd) 
    in 
    let rec split pa pb pc pd =
      let _ = (fun a b c d -> (a, b, c, d)) pa pb pc pd in
			let papb = bottom pc pa pb in
      let pa = papb.ft in
      let pb = papb.sd in
      let pcpd = top pb pc pd in
      let pc = pcpd.ft in
      let pd = pcpd.sd in
      if pb >= pc then {f = pa; s = pb; t = pc; f = pd}
      else 
        let pb' = pb + 1 in
        let pc' = pc - 1 in
      (swap pb pc; 
								 split pa pb' pc' pd) 
 	  in 
    let a' = a + 1 in
    let an = a + n in
    let an' = an - 1 in
    let _ = (fun x y z a b -> (x, y, z, a, b)) a n a' an an' in
    let spllit = split a' a' an' an' in
    (fun x -> x) spllit 
in
let vec = [|6;4;6;8;3;1;4;8;90;4;1;4;7;4|] in
let _ = (fun x -> x) vec in
  quickSort vec 0 13;;
