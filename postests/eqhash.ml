let _ =
  let a0 = Myhash.create 10 in
  let a1 = Myhash.set a0 0 0 in
  let a2 = Myhash.set a1 1 1 in
  let i  = 1 in
  let j  = Myhash.get a2 1 in
    assert (i = j)
