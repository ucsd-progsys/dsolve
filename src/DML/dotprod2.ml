(*qualif LTs(x) : x < Array.length v1;;
qualif LEs(x) : x <= Array.length v1;;
qualif NEs(s) : -.(x = Array.length v1);;
qualif LTs1(x) : x < Array.length v2;;
qualif LEs1(x) : x <= Array.length v2;;
qualif NEs1(s) : -.(x = Array.length v2);;
qualif LT0(x) : x < 0;;
qualif LE0(x) : x <= 0;;
qualif NE0(x) : -.(x = 0);;
qualif GT0(x) : 0 < x;;
qualif GE0(x) : 0 <= x;;
qualif SE3(x) : Array.length x = 3;;*)

qualif Q_AA_LE_ARRAY_LENGTH_V2__(_AA) : _AA <= Array.length v2__;;
qualif Q_AA_GE_ARRAY_LENGTH_V2__(_AA) : _AA >= Array.length v2__;;
qualif Q_AA_EQ_ARRAY_LENGTH_V2__(_AA) : _AA = Array.length v2__;;
qualif Q_AA_NE_ARRAY_LENGTH_V2__(_AA) : _AA != Array.length v2__;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_V2__(_AA) : Array.length _AA <= Array.length v2__;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_V2__(_AA) : Array.length _AA >= Array.length v2__;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_V2__(_AA) : Array.length _AA = Array.length v2__;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_V2__(_AA) : Array.length _AA != Array.length v2__;;
qualif Q_AA_LE_ARRAY_LENGTH_V1__(_AA) : _AA <= Array.length v1__;;
qualif Q_AA_GE_ARRAY_LENGTH_V1__(_AA) : _AA >= Array.length v1__;;
qualif Q_AA_EQ_ARRAY_LENGTH_V1__(_AA) : _AA = Array.length v1__;;
qualif Q_AA_NE_ARRAY_LENGTH_V1__(_AA) : _AA != Array.length v1__;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_V1__(_AA) : Array.length _AA <= Array.length v1__;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_V1__(_AA) : Array.length _AA >= Array.length v1__;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_V1__(_AA) : Array.length _AA = Array.length v1__;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_V1__(_AA) : Array.length _AA != Array.length v1__;;
qualif Q_AA_LE_ARRAY_LENGTH_V2(_AA) : _AA <= Array.length v2;;
qualif Q_AA_GE_ARRAY_LENGTH_V2(_AA) : _AA >= Array.length v2;;
qualif Q_AA_EQ_ARRAY_LENGTH_V2(_AA) : _AA = Array.length v2;;
qualif Q_AA_NE_ARRAY_LENGTH_V2(_AA) : _AA != Array.length v2;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_V2(_AA) : Array.length _AA <= Array.length v2;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_V2(_AA) : Array.length _AA >= Array.length v2;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_V2(_AA) : Array.length _AA = Array.length v2;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_V2(_AA) : Array.length _AA != Array.length v2;;
qualif Q_AA_LE_ARRAY_LENGTH_V1(_AA) : _AA <= Array.length v1;;
qualif Q_AA_GE_ARRAY_LENGTH_V1(_AA) : _AA >= Array.length v1;;
qualif Q_AA_EQ_ARRAY_LENGTH_V1(_AA) : _AA = Array.length v1;;
qualif Q_AA_NE_ARRAY_LENGTH_V1(_AA) : _AA != Array.length v1;;
qualif QARRAY_LENGTH__AA_LE_ARRAY_LENGTH_V1(_AA) : Array.length _AA <= Array.length v1;;
qualif QARRAY_LENGTH__AA_GE_ARRAY_LENGTH_V1(_AA) : Array.length _AA >= Array.length v1;;
qualif QARRAY_LENGTH__AA_EQ_ARRAY_LENGTH_V1(_AA) : Array.length _AA = Array.length v1;;
qualif QARRAY_LENGTH__AA_NE_ARRAY_LENGTH_V1(_AA) : Array.length _AA != Array.length v1;;
qualif Q_AA_LE_SZ_PLUS(_AA) : _AA <= sz_plus;;
qualif Q_AA_GE_SZ_PLUS(_AA) : _AA >= sz_plus;;
qualif Q_AA_EQ_SZ_PLUS(_AA) : _AA = sz_plus;;
qualif Q_AA_NE_SZ_PLUS(_AA) : _AA != sz_plus;;
qualif Q_AA_LE_SZ(_AA) : _AA <= sz;;
qualif Q_AA_GE_SZ(_AA) : _AA >= sz;;
qualif Q_AA_EQ_SZ(_AA) : _AA = sz;;
qualif Q_AA_NE_SZ(_AA) : _AA != sz;;
qualif Q_AA_LE_GET_PROD_I_PLUS_DEREF_SUM(_AA) : _AA <= get_prod_i_plus_deref_sum;;
qualif Q_AA_GE_GET_PROD_I_PLUS_DEREF_SUM(_AA) : _AA >= get_prod_i_plus_deref_sum;;
qualif Q_AA_EQ_GET_PROD_I_PLUS_DEREF_SUM(_AA) : _AA = get_prod_i_plus_deref_sum;;
qualif Q_AA_NE_GET_PROD_I_PLUS_DEREF_SUM(_AA) : _AA != get_prod_i_plus_deref_sum;;
qualif Q_AA_LE_DEREF_SUM(_AA) : _AA <= deref_sum;;
qualif Q_AA_GE_DEREF_SUM(_AA) : _AA >= deref_sum;;
qualif Q_AA_EQ_DEREF_SUM(_AA) : _AA = deref_sum;;
qualif Q_AA_NE_DEREF_SUM(_AA) : _AA != deref_sum;;
qualif Q_AA_LE_GET_PROD_I(_AA) : _AA <= get_prod_i;;
qualif Q_AA_GE_GET_PROD_I(_AA) : _AA >= get_prod_i;;
qualif Q_AA_EQ_GET_PROD_I(_AA) : _AA = get_prod_i;;
qualif Q_AA_NE_GET_PROD_I(_AA) : _AA != get_prod_i;;
qualif Q_AA_LE_GET_V2_I(_AA) : _AA <= get_v2_i;;
qualif Q_AA_GE_GET_V2_I(_AA) : _AA >= get_v2_i;;
qualif Q_AA_EQ_GET_V2_I(_AA) : _AA = get_v2_i;;
qualif Q_AA_NE_GET_V2_I(_AA) : _AA != get_v2_i;;
qualif Q_AA_LE_GET_V1_I(_AA) : _AA <= get_v1_i;;
qualif Q_AA_GE_GET_V1_I(_AA) : _AA >= get_v1_i;;
qualif Q_AA_EQ_GET_V1_I(_AA) : _AA = get_v1_i;;
qualif Q_AA_NE_GET_V1_I(_AA) : _AA != get_v1_i;;
qualif Q_AA_LE_DEREF_I_PLUS(_AA) : _AA <= deref_i_plus;;
qualif Q_AA_GE_DEREF_I_PLUS(_AA) : _AA >= deref_i_plus;;
qualif Q_AA_EQ_DEREF_I_PLUS(_AA) : _AA = deref_i_plus;;
qualif Q_AA_NE_DEREF_I_PLUS(_AA) : _AA != deref_i_plus;;
qualif Q_AA_LE_DEREF_I(_AA) : _AA <= deref_i;;
qualif Q_AA_GE_DEREF_I(_AA) : _AA >= deref_i;;
qualif Q_AA_EQ_DEREF_I(_AA) : _AA = deref_i;;
qualif Q_AA_NE_DEREF_I(_AA) : _AA != deref_i;;
qualif Q_AA_LE_SZ(_AA) : _AA <= sz;;
qualif Q_AA_GE_SZ(_AA) : _AA >= sz;;
qualif Q_AA_EQ_SZ(_AA) : _AA = sz;;
qualif Q_AA_NE_SZ(_AA) : _AA != sz;;
qualif Q_AA_LE__NONE(_AA) : _AA <= _none;;
qualif Q_AA_GE__NONE(_AA) : _AA >= _none;;
qualif Q_AA_EQ__NONE(_AA) : _AA = _none;;
qualif Q_AA_NE__NONE(_AA) : _AA != _none;;
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
qualif QARRAY_LENGTH__AA_NE_1(_AA) : Array.length _AA != 1;;


let dotprod v1 v2 = 
  let sz = Array.length v1 in
  let sum = ref 0 in
	let i = ref 0 in
	let rec loop _none = 
		let deref_i = !i in
		let deref_i_plus = deref_i + 1 in
		if deref_i < sz then   
     		(	let get_v1_i = Array.get v1 deref_i in	
					let get_v2_i = Array.get v2 deref_i in
					let get_prod_i = get_v1_i * get_v2_i in
					let deref_sum = !sum in
					let get_prod_i_plus_deref_sum = get_prod_i + deref_sum in
					sum := get_prod_i_plus_deref_sum; 
					i := deref_i_plus;
					loop () ) 
		else ()
    in (loop (); !sum)
in
let _none = Random.init 555 in
let sz = Random.int 40 in
let sz_plus = sz + 1 in
let v1__ = Array.make sz_plus 1 in
let v2__ = Array.make sz_plus 1 in
dotprod v1__ v2__ 
;;



(*
let dotprod v1 v2 = begin
  let sum = ref 0 in
    for i = 0 to pred (vect_length v1) do
      sum := v1..(i) * v2..(i) + !sum
    done;
    !sum
end withtype {n:nat} int vect(n) -> int vect(n) -> int
;;
*)
