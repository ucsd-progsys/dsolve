
#datatype 'a array2D with (nat,nat) =
#  {m:nat,n:nat} A(m,n) of ('a array(n)) array(m) * int(m) * int(n)

fun('a) nRows (A (_, m, _)) = m
#withtype {m:nat,n:nat} <> => 'a array2D(m,n) -> int(m)

fun('a) nCols (A (_, _, n)) = n
#withtype {m:nat,n:nat} <> => 'a array2D(m,n) -> int(n)


fun is_neg_aux (arr2, n, j) =
    if j < n - 1 then
	if sub2 (arr2, 0, j) <. 0.0 then true
	else is_neg_aux (arr2, n, j+1)  
    else false
#withtype {m:pos,n:pos,j:nat | j <= n} <n-j> =>
#         (float array(n)) array(m) * int(n) * int(j) -> bool

fun is_neg (arr2, n) = is_neg_aux (arr2, n, 1)
#withtype {m:pos,n:pos} <> => (float array(n)) array(m) * int(n) -> bool


fun unb1 (arr2, m, n, i, j) = 
    if j < n-1 then
	if sub2 (arr2, 0, j) <. 0.0 then unb2 (arr2, m, n, i+1, j)
	else unb1 (arr2, m, n, 0, j+1)
    else false
#withtype {m:pos,n:pos,i:nat,j:nat | i < m, j <= n} <n-j, m-i> =>
#         (float array(n)) array(m) * int (m) * int(n) * int(i) * int(j) -> bool

and unb2 (arr2, m, n, i, j) =
    if i < m then
	if sub2 (arr2, i, j) <. 0.0 then unb2 (arr2, m, n, i+1, j)
	else unb1 (arr2, m, n, 0, j+1)
    else true
#withtype {m:pos,n:pos,i:nat,j:nat | i <= m, j < n} <n-j,m-i> =>
#         (float array(n)) array(m) * int (m) * int(n) * int(i) * int(j) -> bool



fun enter_var (arr2, n, j, c, j') = 
    if j' < n-1 then
	let
	    val c' = sub2 (arr2, 0, j')
	in
	    if c' <. c then enter_var (arr2, n, j', c', j'+1)
	    else enter_var (arr2, n, j, c, j'+1)
	end
    else j
#withtype {m:pos,n:pos,j:pos,j':pos | j+1 < n, j' < n} <n-j'> =>
#         (float array(n)) array(m) * int(n) * int(j) * float * int(j') ->
#	 [j:pos | j+1 < n] int(j)


fun depart_var (arr2, m, n, j, i, r, i') =
    if i' < m then
	let
	    val c' = sub2 (arr2, i', j)
	in
	    if c' >. 0.0 then
		let
		    val r' = sub2(arr2, i', n-1) /. c'
		in
		    if r' <. r then depart_var(arr2, m, n, j, i', r', i'+1)
		    else depart_var (arr2, m, n, j, i, r, i'+1)
		end
	    else depart_var (arr2, m, n, j, i, r, i'+1)
	end
    else i
#withtype {m:pos,n:pos,i:pos,i':pos,j:pos | i < m, i' <= m, j < n} <m-i'> =>
#         (float array(n)) array(m) * int(m) * int(n) * int(j) * int(i) * float * int(i') ->
#	 [i:pos | i < m] int(i)

fun init_ratio (arr2, m, n, j, i) =
  if i < m then
      let
	  val c = sub2 (arr2, i, j)
      in
	  if c >. 0.0 then (i, sub2 (arr2, i, n-1) /. c)
	  else init_ratio (arr2, m, n, j, i+1)
      end
  else abort ("init_ratio: negative coefficients!")
#withtype {m:pos,n:pos,j:pos,i:pos | j < n, i <= m} <m-i> =>
#         (float array(n)) array(m) * int(m) * int(n) * int(j) * int(i) ->
#         [i:pos | i < m] int(i) * float


fun norm_aux (arr2, n, i, c, j) =
  if j < n then
    let
        val _ = update2 (arr2, i, j, sub2 (arr2, i, j) /. c)
    in
        norm_aux (arr2, n, i, c, j+1)
    end
  else ()
#withtype {m:pos,n:pos,i:pos,j:pos | i < m, j <= n} <n-j> =>
#         (float array(n)) array(m) * int(n) * int(i) * float * int(j) -> unit

fun norm (arr2, n, i, j) =
  let
      val c = sub2 (arr2, i, j)
  in
      norm_aux (arr2, n, i, c, 1)
  end
#withtype {m:pos,n:pos,i:pos,j:pos | i < m, j < n} <> =>
#         (float array(n)) array(m) * int(n) * int(i) * int(j) -> unit

fun row_op_aux1 (arr2, n, i, i', c, j) =
  if j < n then 
      let
	  val cj =  sub2 (arr2, i, j)
	  val cj' =  sub2 (arr2, i', j)
	  val _ = update2 (arr2, i', j, cj' -. cj *. c)
      in
	  row_op_aux1 (arr2, n, i, i', c, j+1)
      end
  else ()
#withtype {m:pos,n:pos,i:pos,i':nat, j:pos | i < m, i' < m, j <= n} <n-j> =>
#         (float array(n)) array(m) * int(n) * int(i) * int(i') * float * int(j) -> unit

fun row_op_aux2 (arr2, n, i, i', j) =
  let
      val c' = sub2 (arr2, i', j)
  in
      row_op_aux1 (arr2, n, i, i', c', 1)
  end
#withtype {m:pos,n:pos,i:pos,i':nat, j:pos | i < m, i' < m, j < n} <> =>
#         (float array(n)) array(m) * int(n) * int(i) * int(i') * int(j) -> unit

fun row_op_aux3 (arr2, m, n, i, j, i') =
  if i' < m then
     if i' <> i then
	 let
	     val _ = row_op_aux2(arr2, n, i, i', j)
	 in
	     row_op_aux3 (arr2, m, n, i, j, i'+1)
         end
     else row_op_aux3 (arr2, m, n, i, j, i'+1)
  else ()
#withtype {m:pos,n:pos,i:pos,j:pos,i':nat | i < m, j < n, i' <= m} <m-i'> =>
#         (float array(n)) array(m) * int(m) * int(n) * int(i) * int(j) * int(i') -> unit

fun row_op (arr2, m, n, i, j) =
    let
	val _ = norm (arr2, n, i, j)
    in
	row_op_aux3 (arr2, m, n, i, j, 0)
    end
#withtype {m:pos,n:pos,i:pos,j:pos| i < m, j < n} <> =>
#         (float array(n)) array(m) * int(m) * int(n) * int(i) * int(j) -> unit

fun simplex (arr2, m, n) =
    if is_neg (arr2, n) then
	if unb1 (arr2, m, n, 0, 1) then abort ("simplex: unbound solution!")
	else
	    let
		val j = enter_var (arr2, n, 1, sub2 (arr2, 0, 1), 2)
		val (i, r) = init_ratio (arr2, m, n, j, 1)
		val i = depart_var  (arr2, m, n, j, i, r, i+1)
		val _ = row_op (arr2, m, n, i, j)
	    in
		simplex (arr2, m, n)
	    end
    else ()
#withtype {m:int,n:int | m > 1, n > 2}
#         (float array(n)) array(m) * int(m) * int(n) -> unit

fun main (A (arr2, m, n)) =
  if m > 1 then
      if n > 2 then simplex (arr2, m, n)
      else abort ("too few columns")
  else abort ("too few rows")
#withtype float array2D -> unit
