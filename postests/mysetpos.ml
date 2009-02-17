let x = Myset.empty
let eq = Myset.eq
let y = eq x x

let _ = assert (y)

let f a b =
   assert (eq (Myset.cup a b) (Myset.cup b a))

let f a b =
   assert (eq (Myset.cup a b) (Myset.cup b a))

let f a = 
   assert (eq (Myset.cup a x) a)

let f a = 
   assert (eq (Myset.cap a x) x)

let f a = 
   assert (eq (Myset.cup a a) a)

let f a = 
   assert (eq (Myset.cap a a) a)

let f a b = 
   if eq a b then
    assert (eq (Myset.cup a b) a)
   else assert false

let f a b =
   if eq a b then
    assert (eq (Myset.cap a b) a)
   else assert false

let f z a =
   if Myset.mem z a then
    assert (eq (Myset.cup a (Myset.sng z)) a)
   else assert false

let f z a b =
   if eq a b then
    assert (eq (Myset.cup a (Myset.sng z)) (Myset.cup b (Myset.sng z)))
   else assert false

let f z a b =
   if eq (Myset.cap a b) Myset.empty && Myset.mem z a && Myset.mem z b then
     assert (1 = 0)
   else assert false

let f z a b =
   if eq (Myset.mns a b) a && Myset.mem z a then
     assert (not (Myset.mem z b))
   else assert false

let f z a b c =
   if not (Myset.mem z a) && eq (Myset.cup b c) a then
     assert (not (Myset.mem z b))
   else assert false
