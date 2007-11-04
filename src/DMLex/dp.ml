(*qualif Q_AA_LE_ARRAY_LENGTH_ARR(_AA) : _AA <= Array.length arr;;
qualif Q_AA_GE_ARRAY_LENGTH_ARR(_AA) : _AA >= Array.length arr;;
qualif Q_AA_EQ_ARRAY_LENGTH_ARR(_AA) : _AA = Array.length arr;;
qualif Q_AA_NE_ARRAY_LENGTH_ARR(_AA) : _AA != Array.length arr;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_ARR(_AA) : Array.length _AA <= Array.length arr;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_ARR(_AA) : Array.length _AA >= Array.length arr;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_ARR(_AA) : Array.length _AA = Array.length arr;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_ARR(_AA) : Array.length _AA != Array.length arr;;
qualif Q_AA_LE_ARRAY_LENGTH_B(_AA) : _AA <= Array.length b;;
qualif Q_AA_GE_ARRAY_LENGTH_B(_AA) : _AA >= Array.length b;;
qualif Q_AA_EQ_ARRAY_LENGTH_B(_AA) : _AA = Array.length b;;
qualif Q_AA_NE_ARRAY_LENGTH_B(_AA) : _AA != Array.length b;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_B(_AA) : Array.length _AA <= Array.length b;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_B(_AA) : Array.length _AA >= Array.length b;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_B(_AA) : Array.length _AA = Array.length b;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_B(_AA) : Array.length _AA != Array.length b;;
qualif Q_AA_LE_ARRAY_LENGTH_A(_AA) : _AA <= Array.length a;;
qualif Q_AA_GE_ARRAY_LENGTH_A(_AA) : _AA >= Array.length a;;
qualif Q_AA_EQ_ARRAY_LENGTH_A(_AA) : _AA = Array.length a;;
qualif Q_AA_NE_ARRAY_LENGTH_A(_AA) : _AA != Array.length a;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_A(_AA) : Array.length _AA <= Array.length a;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_A(_AA) : Array.length _AA >= Array.length a;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_A(_AA) : Array.length _AA = Array.length a;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_A(_AA) : Array.length _AA != Array.length a;;
qualif Q_AA_LE_P(_AA) : _AA <= p;;
qualif Q_AA_GE_P(_AA) : _AA >= p;;
qualif Q_AA_EQ_P(_AA) : _AA = p;;
qualif Q_AA_NE_P(_AA) : _AA != p;;
qualif Q_AA_LE_SUM'(_AA) : _AA <= sum';;
qualif Q_AA_GE_SUM'(_AA) : _AA >= sum';;
qualif Q_AA_EQ_SUM'(_AA) : _AA = sum';;
qualif Q_AA_NE_SUM'(_AA) : _AA != sum';;
qualif Q_AA_LE_SUBM(_AA) : _AA <= subm;;
qualif Q_AA_GE_SUBM(_AA) : _AA >= subm;;
qualif Q_AA_EQ_SUBM(_AA) : _AA = subm;;
qualif Q_AA_NE_SUBM(_AA) : _AA != subm;;
qualif Q_AA_LE_SBI(_AA) : _AA <= sbi;;
qualif Q_AA_GE_SBI(_AA) : _AA >= sbi;;
qualif Q_AA_EQ_SBI(_AA) : _AA = sbi;;
qualif Q_AA_NE_SBI(_AA) : _AA != sbi;;
qualif Q_AA_LE_SAI(_AA) : _AA <= sai;;
qualif Q_AA_GE_SAI(_AA) : _AA >= sai;;
qualif Q_AA_EQ_SAI(_AA) : _AA = sai;;
qualif Q_AA_NE_SAI(_AA) : _AA != sai;;
qualif Q_AA_LE_I'(_AA) : _AA <= i';;
qualif Q_AA_GE_I'(_AA) : _AA >= i';;
qualif Q_AA_EQ_I'(_AA) : _AA = i';;
qualif Q_AA_NE_I'(_AA) : _AA != i';;
qualif Q_AA_LE_SUM(_AA) : _AA <= sum;;
qualif Q_AA_GE_SUM(_AA) : _AA >= sum;;
qualif Q_AA_EQ_SUM(_AA) : _AA = sum;;
qualif Q_AA_NE_SUM(_AA) : _AA != sum;;
qualif Q_AA_LE_I(_AA) : _AA <= i;;
qualif Q_AA_GE_I(_AA) : _AA >= i;;
qualif Q_AA_EQ_I(_AA) : _AA = i;;
qualif Q_AA_NE_I(_AA) : _AA != i;;
qualif Q_AA_LE_N(_AA) : _AA <= n;;
qualif Q_AA_GE_N(_AA) : _AA >= n;;
qualif Q_AA_EQ_N(_AA) : _AA = n;;
qualif Q_AA_NE_N(_AA) : _AA != n;;
qualif Q_AA_LE_0(_AA) : _AA <= 0;;
qualif Q_AA_GE_0(_AA) : _AA >= 0;;
qualif Q_AA_EQ_0(_AA) : _AA = 0;;
qualif Q_AA_NE_0(_AA) : _AA != 0;;
qualif QARRAY_LENGTH__AA_LE_0(_AA) : Array.length _AA <= 0;;
qualif QARRAY_LENGTH__AA_GE_0(_AA) : Array.length _AA >= 0;;
qualif QARRAY_LENGTH__AA_EQ_0(_AA) : Array.length _AA = 0;;
qualif QARRAY_LENGTH__AA_NE_0(_AA) : Array.length _AA != 0;;
qualif Q_AA_LE_1(_AA) : _AA <= 1;;
qualif Q_AA_GE_1(_AA) : _AA >= 1;;
qualif Q_AA_EQ_1(_AA) : _AA = 1;;
qualif Q_AA_NE_1(_AA) : _AA != 1;;
qualif QARRAY_LENGTH__AA_LE_1(_AA) : Array.length _AA <= 1;;
qualif QARRAY_LENGTH__AA_GE_1(_AA) : Array.length _AA >= 1;;
qualif QARRAY_LENGTH__AA_EQ_1(_AA) : Array.length _AA = 1;;
qualif QARRAY_LENGTH__AA_NE_1(_AA) : Array.length _AA != 1;;*)

(* just dotprod again *)
(*let dp a b =
  let n = Array.length a in
  let rec loop i sum =
    let i' = i + 1 in
    let sai = Array.get a i in
    let sbi = Array.get b i in   
    let subm = sai + sbi in
    let sum' = subm + sum in
    if i = n then sum else  
      loop i' sum'
     (*withtype {i:nat | i <= n} <n-i> => int(i) * int -> int*)
  in
     loop 0 0
in 
let none_ = Random.self_init () in
let p = Random.int 10 in
let p = p + 1 in
let arr = Array.make p 1 in
dp arr arr;;*)

(*withtype {n:nat} int array(n) * int array(n) -> int*)

(* some infinite looking recursion thing *)

(*let rec ack m n =
  let n' = n + 1 in
  if m = 0 then n'
  else
	  let ackm n = 
      let m' = m - 1 in 
      ack m' n
          (*withtype {n:nat} <> => int(n) -> [a:nat] int(a)*)
      in
	  if n = 0 then ackm 1 else 
      let n' = n - 1 in
      let ackmn' = ack m n' in
      ackm ackmn'
in 
let none_ = Random.self_init () in
let m = Random.int 10 in
let n = Random.int 10 in
ack m n
;;*)
(*withtype {m:nat} int(m) -> {n:nat} <m, n> => int(n) -> [a:nat] int(a)*)

(*fun dp (a, b) =
  let
     val n = arraysize a
     fun loop (i, sum) =
       if i = n then sum else loop (i+1, sum + sub (a, i) * sub (b, i))
     withtype {i:nat | i <= n} <n-i> => int(i) * int -> int
  in
     loop (0, 0)
  end  
withtype {n:nat} int array(n) * int array(n) -> int

fun ack m n =
  if m = 0 then n+1
  else
      let
	  fun ackm n = ack (m-1) n
          withtype {n:nat} <> => int(n) -> [a:nat] int(a)
      in
	  if n = 0 then ackm 1 else ackm (ack m (n-1))
      end
withtype {m:nat} int(m) -> {n:nat} <m, n> => int(n) -> [a:nat] int(a)
*)
(*
fun dp {n:nat} (a: int array(n), b: int array(n)): int =
  let
     val n = arraysize a
     fun loop {i:nat | i < n} (i: int(i), sum: int) =
       if i = n then sum else loop (i+1, sum  + sub (a, i) * sub (b, i))
  in
     loop (0, 0)
  end  

fun zip {n:nat} (xs: 'a list(n) , ys: 'b list(n)): ('a * 'b) list(n) =
  case (a, b) of
    ([], []) => []
  | (x :: xs, y :: ys) => (x, y) :: zip (xs, ys)

fun ack {i:nat} (m:int(i)) {j:nat} (n:int(j)): int =
  if m = 0 then n+1
  else if n = 0 then ack (m-1) 1
       else ack (m-1) (ack m (n-1))
withtype {i:nat} int(i) -> {j:nat} int(j) -> int

fun ack (m:int(i)) (n:int(j)): int =
  if m = 0 then n+1
  else if n = 0 then ack (m-1) 1
       else ack (m-1) (ack m (n-1))
withtype {i:nat} int(i) -> {j:nat} int(j) -> int

*)
