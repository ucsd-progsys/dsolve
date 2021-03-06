let f (i: int) =
  let id x = x in
    id true
in f 0


(*let sort cmp arr =
  let rec qsort lo hi =
    if hi - lo >= 6 then begin
      let mid = (lo + hi) lsr 1 in
      (* Select median value from among LO, MID, and HI. Rearrange
         LO and HI so the three values are sorted. This lowers the
         probability of picking a pathological pivot.  It also
         avoids extra comparisons on i and j in the two tight "while"
         loops below. *)
      if cmp (Array.get arr mid) (Array.get arr lo) then swap arr mid lo;
      if cmp (Array.get arr hi) (Array.get arr mid) then begin
        swap arr mid hi;
        if cmp (Array.get arr mid) (Array.get arr lo) then swap arr mid lo
      end;
      let pivot = Array.get arr mid in
      let i = ref (lo + 1) and j = ref (hi - 1) in
      if not (cmp pivot (Array.get arr hi))
         || not (cmp (Array.get arr lo) pivot)
      then assert false;
      while !i < !j do
        while not (cmp pivot (Array.get arr !i)) do incr i done;
        while not (cmp (Array.get arr !j) pivot) do decr j done;
        if !i < !j then swap arr !i !j;
        incr i; decr j
      done;
      (* Recursion on smaller half, tail-call on larger half *)
      if !j - lo <= hi - !i then begin
        qsort lo !j; qsort !i hi
      end else begin
        qsort !i hi; qsort lo !j
      end
    end in
  qsort 0 (Array.length arr - 1);
  (* Finish sorting by insertion sort *)
  for i = 1 to Array.length arr - 1 do
    let val_i = (Array.get arr i) in
    if not (cmp (Array.get arr (i - 1)) val_i) then begin
      Array.set arr i (Array.get arr (i - 1));
      let j = ref (i - 1) in
      while !j >= 1 && not (cmp (Array.get arr (!j - 1)) val_i) do
        Array.set arr !j (Array.get arr (!j - 1));
        decr j
      done;
      Array.set arr !j val_i
    end
  done
in 
  sort (<=) vec; vec;;
 *)
