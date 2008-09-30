let test0 = 
  let a = Mystore.make 10 0 in
  assert (0 = Mystore.get a 5)

let test1 a = 
  let a' = Mystore.set a 0 0 in
  assert (0 = Mystore.get a' 0)

let test2 a = 
  let v1 = Mystore.get a 4 in
  let a' = Mystore.set a 5 22 in
  let v2 = Mystore.get a' 4 in
  let v3 = Mystore.get a' 5 in
  assert (v1 = v2); (* PASS *)
  assert (v3 = 22)

let test3 a i1 i2 =
  if not (i1 = i2) then
    let v1 = Mystore.get a  i1 in
    let a' = Mystore.set a  i2 22 in
    let v2 = Mystore.get a' i1 in
    let v3 = Mystore.get a' i2 in
    assert (v1 = v2); (* PASS *)
    assert (v3 = 22)
  else ()

let test4 god p r0 r1 x y i vv =
  if (r1 = Mystore.set r0 x (1 + Mystore.get r0 x)) &&
     (not (x = god)) &&
     (god = Mystore.get p x) &&
     (vv  = Mystore.get p i) &&
     (vv = god || Mystore.get r0 i < Mystore.get r0 vv) 
  then
     assert (vv = god || Mystore.get r1 i < Mystore.get r1 vv)
  else ()
