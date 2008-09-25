let x = Myset.empty
let y = Myset.eq x x

let _ = assert (y)

let f a b =
   assert (Myset.eq (Myset.cup a b) (Myset.cup b a))

let f a b =
   assert (Myset.eq (Myset.cup a b) (Myset.cup b a))

let f a = 
   assert (Myset.eq (Myset.cup a x) a)

let f a = 
   assert (Myset.eq (Myset.cap a x) x)

let f a = 
   assert (Myset.eq (Myset.cup a a) a)

let f a = 
   assert (Myset.eq (Myset.cap a a) a)

let f a b = 
   if Myset.eq a b then
    assert (Myset.eq (Myset.cup a b) a)
   else assert false

let f a b =
   if Myset.eq a b then
    assert (Myset.eq (Myset.cap a b) a)
   else assert false

let f z a =
   if Myset.mem z a then
    assert (Myset.eq (Myset.cup a (Myset.sng z)) a)
   else assert false

let f z a b =
   if Myset.eq a b then
    assert (Myset.eq (Myset.cup a (Myset.sng z)) (Myset.cup b (Myset.sng z)))
   else assert false


