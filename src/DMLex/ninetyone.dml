qualif EQ91(x) : x = 91;;
qualif LE100(x) : x <= 100;;
qualif TEN(x) : x = 10;;
qualif EQ1000(x) : x = 1000;;


let rec f91 x =
  let x_minus_10 = x - 10 in
  let x_plus_11 = x + 11 in
  if (x <= 100) then 
    let f_x_plus_11 = f91 x_plus_11 in
    f91 f_x_plus_11    
  else
    x_minus_10 in f91 99;;
(*withtype {i:int} <max(0, 101-i)> => 
         int(i) -> 
         [j:int | (i<=100 /\ j=91) \/ (i>=101 /\ j=i-10)] int(j)*)
