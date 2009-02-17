let x = Myset.empty

let f a b =
   assert (Myset.eq (Myset.cap a b) (Myset.cup b a))

let f a b =
   assert (Myset.eq (Myset.cap a b) (Myset.cup b a))

let f a =
   assert (Myset.eq x a)

let f a b =
   assert (Myset.eq (Myset.cup a b) a)

let f a b =
   assert (Myset.eq (Myset.cap a b) a)

let f z a =
   assert (Myset.eq (Myset.cup (Myset.sng z) a) a)
