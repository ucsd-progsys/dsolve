let r_1 = 0

let rec (f, r_2) = (fun x r_4 -> x, r_4), r_1

let f_assert1 x_a1_1 resources_a1_1 = 
  let (_, resources_a1_2) = f x_a1_1 resources_a1_1 in
    assert(resources_a1_2 = resources_a1_1);;
