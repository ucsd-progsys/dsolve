qualifier LTs(x) = x < size queenArray;;
qualifier LEs(x) = x <= size queenArray;;
qualifier NEs(s) = not(x = size queenArray);;
qualifier LT0(x) = x < 0;;
qualifier LE0(x) = x <= 0;;
qualifier NE0(x) = not(x = 0);;
qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier BOOL(x) = (x = 0) or (x = 1);;
qualifier TRUE(x) = x = 1;;
qualifier FALSE(x) = x = 0;;


let abs _x_ = if _x_ < 0 then let _y_ = 0 - _x_ in _y_ else _x_ in
let queen _1_size =
begin
  let queenArray = make _1_size 0 in
  let assign _1_i _1_j = set queenArray _1_i _1_j in
  let rec dotsPrint _2_n = 
			begin
			if _2_n = 0 then () else 
				let _2_n_minus = _2_n - 1 in dotsPrint _2_n_minus
			end
	in  
  let queenPrint _none =
	begin
    let rec _1_aux _1_row = 
		begin
      if _1_row = _1_size then () else
      let _1_n = get queenArray _1_row in 
			let _1_n_minus = _1_n - 1 in 
			let _1_size_minus_1_n = _1_size - _1_n in 
			let _1_row_plus = _1_row + 1 in
			dotsPrint _1_n_minus; dotsPrint _1_size_minus_1_n; _1_aux _1_row_plus 
    end
    in _1_aux 0
	end 
  in
  let test _2_j =
    let q2j = get queenArray _2_j  in
    let rec _2_aux _2_i =
		begin
      if _2_i < _2_j then
        let q2i = get queenArray _2_i in
				let qdiff = q2j - q2i in
				let absqdiff = abs qdiff in
        if q2i = q2j then false else if absqdiff = _2_j - _2_i then false else 
					let _2_i_plus = _2_i + 1 in _2_aux _2_i_plus
      else true
		end
    in _2_aux 0  
  in
  let rec loop _2_row =
		let _2_row_minus = _2_row - 1 in
		let _2_row_plus = _2_row + 1 in
		let _get_queenArray_2_row = get queenArray _2_row in
    let next = _get_queenArray_2_row + 1 in
      if _1_size < next then
        begin assign _2_row 0; if _2_row = 0 then () else loop _2_row_minus end
      else
      begin 
				assign _2_row next;
        if test _2_row then
				begin
          if _2_row_plus = _1_size then begin queenPrint (); loop _2_row end else loop _2_row_plus
				end
        else loop _2_row
      end  
	in loop 0
end 
in queen 10 ;;




(*let{size:int | 0 < size}
queen(size) =
  let queenArray = make_vect size 0 in
  let assign i j = queenArray..(i) <- j
  withtype {i:nat | i < size} int(i) -> int -> unit in
  let rec dotsPrint n = if n = 0 then () else begin print_string "."; dotsPrint (n-1) end in
  let queenPrint () =
    let rec aux row = begin
      if eq_int row size then () else
      let n = queenArray..(row) in
        dotsPrint(n-1); print_string "Q"; dotsPrint(size - n); print_string "\n"; aux (row + 1)
      end
    withtype {i:nat | i <= size } int(i) -> unit in
    aux(0); print_string "\n" in
  let test j =
    let qj = queenArray..(j) in
    let rec aux i =
      if lt_int i j then
        let qi = queenArray..(i) in
        if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
      else true
    withtype {i:nat | i < size} int(i) -> bool in aux 0
  withtype {j:nat | j < size} int(j) -> bool in
  let rec loop row =
    let next = queenArray..(row) + 1 in
      if next > size then
        begin assign row 0; if eq_int row 0 then () else loop (row-1) end
      else
      begin assign row next;
        if test row then
          if eq_int (row+1) size then begin queenPrint(); loop(row) end else loop(row+1)
          else loop row
      end
  withtype {row:nat | row < size} int(row) -> unit in loop(0)
withtype int(size) -> unit
;;*)