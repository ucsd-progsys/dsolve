let x = Myset.empty
let y = Myset.empty2
let xx = 1
let yy = 2
let zz = Myset.add xx x
let zzz = Myset.add xx y
let _ = assert (zz = zzz)
