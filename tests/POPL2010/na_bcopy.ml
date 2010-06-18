let rec bcopy_aux src des i m =
  if i = m then 
    () 
  else begin
    Junkarray.set des i (Junkarray.get src i);
    bcopy_aux src des (i+1) m
  end

let bcopy src des =
  let sz = Junkarray.length src in
  bcopy_aux src des 0 sz

(********************* Driver **********************)

let sz = read_int ()

if sz <= 0 then () else
  let src = Junkarray.init sz (fun i -> i) in
  let dst = Junkarray.make sz 0 in
  bcopy src dst
