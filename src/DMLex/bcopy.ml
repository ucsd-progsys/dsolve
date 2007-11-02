qualif NNeg(x): 0 <= x;;
qualif Eq3(x): x = 3;;
qualif LtLenSr(x): x < Array.length sr;
qualif LtLenSrc(x): x < Array.length src;;
qualif LeLenSr(x): x <= Array.length sr;;
qualif LeLenSrc(x): x <= Array.length src;;
qualif EqLenSrc(x): x = Array.length src;;
qualif EqSrcLen(x): Array.length x = Array.length src;;
qualif LtM(x): x < m;;

let rec bcopy_aux src des i m =
  if i = m then ()
  else
    begin
      let n = Array.get src i in
        Array.set des i n;
        let j = i + 1 in
          bcopy_aux src des j m
    end
in
let bcopy src des =
  let sz = Array.length src in
    bcopy_aux src des 0 sz
in
let sr = [|1; 2; 3|] in
let asz = 3 in
let ds = Array.make asz 0 in
  bcopy sr ds;;
