let t = ref (0, 1)

let get_b a =
  let (a', b') = !t in
    assume (a = a');
    b'

let check a =
  let b = get_b a in
    assert (a < b)
