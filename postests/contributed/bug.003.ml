let rec mlength l =
  match l with
    | [] -> 0
    | _ :: t -> 1 + mlength t

let rec ins n1 l1 y1 =
  match l1 with
    | [] -> (n1 :: [], y1 + 1)
    | h1 :: t1 ->
        let len1 = mlength t1 in
          if n1 <= h1
          then
            begin
              (n1 :: h1 :: t1, y1 + 2)
            end
          else
            let (rec_res1, y1') = ins n1 t1 y1 in
              (h1 :: rec_res1, y1' + 1);;

let f_assert1 n_a1 l_a1 y_a1 =
  let (_, y_a1') = ins n_a1 l_a1 y_a1 in
    assert(y_a1' <= y_a1 + 1 + mlength l_a1);;

let rec sort l2 y2 =
  match l2 with
    | [] -> 
        [], y2
    | h2 :: t2 ->

        let len2 = mlength l2 in

        let sort_res1, y2' = sort t2 y2 in
        let ins_res1, y2'' = ins h2 sort_res1 y2' in

          assert(mlength sort_res1 = mlength t2);
          assert(mlength l2 = 1 + mlength sort_res1);
          assert(mlength l2 = mlength ins_res1);

(*
          assert(y2'' <= (y2 + (((mlength l2) * ((mlength l2) + 1)) / 2)));
*)

          ins_res1, y2''


(*
let f_assert2 l_a2 y_a2 =
  let len3 = mlength l_a2 in
  let (_, y_a2') = sort l_a2 y_a2 in
    assert(y_a2' <= (y_a2 + (((mlength l_a2) * ((mlength l_a2) + 1)) / 2)));;
*)
