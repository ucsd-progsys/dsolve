let swap arr i j =
  let tmp = Array.get arr i in
    (Array.set arr i (Array.get arr j);
  Array.set arr j tmp)
in

let ffor s d body =
  let rec dofor i =
    if i <= d then (body i; dofor (i+1)) else ()
  in dofor s
in

(*let fwhile b body =
  let rec whiledo _n =
    if b () then (body (); whiledo ()) else ()
  in whiledo ()
in

let incr i =
  i := !i + 1
in
let decr i =
  i := !i - 1
in*)

let sort cmp arr =
  let rec qsort lo hi = 
    if hi - lo >= 6 then begin 
      let mid = (lo + hi)/2 in
      (* Select median value from among LO, MID, and HI. Rearrange
         LO and HI so the three values are sorted. This lowers the
         probability of picking a pathological pivot.  It also
         avoids extra comparisons on i and j in the two tight "while"
         loops below. *)
      if cmp (Array.get arr mid) (Array.get arr lo) then swap arr mid lo else ();
      if cmp (Array.get arr hi) (Array.get arr mid) then begin
        swap arr mid hi;
        if cmp (Array.get arr mid) (Array.get arr lo) then swap arr mid lo else ()
      end else ();
      let pivot = Array.get arr mid in
      if not (cmp pivot (Array.get arr hi))
         || not (cmp (Array.get arr lo) pivot)
      then assert false else ();
      let rec bod1 i j = 
        if i < j then 
          let i =
            let rec bod2 i = 
              if cmp pivot (Array.get arr i) 
                then bod2 (i+1) else i
            in bod2 i in
          let j =
            let rec bod3 j =
            if cmp (Array.get arr j) pivot 
                then bod3 (j-1) else j
            in bod3 j in
              (if i < j then swap arr i j else ();
               bod1 (i+1) (j-1))
         else
      (* Recursion on smaller half, tail-call on larger half *)
         if j - lo <= hi - i then begin
           qsort lo j; qsort i hi
         end else begin
           qsort i hi; qsort lo j
         end
      in bod1 (lo+1) (hi-1);
    end 
    else () 
  in qsort 0 (Array.length arr - 1);
  (* Finish sorting by insertion sort *)
  let forbod i = 
    let val_i = (Array.get arr i) in
    if not (cmp (Array.get arr (i - 1)) val_i) then begin
      Array.set arr i (Array.get arr (i - 1));
      let rec wbod j =
          if j >= 1 then if not (cmp (Array.get arr (j - 1)) val_i) then
            (Array.set arr j (Array.get arr (j-1));
            wbod (j - 1)) else Array.set arr j val_i else Array.set arr j val_i
      in wbod (i-1);
    end else ()
  in ffor 1 (Array.length arr - 1) forbod
in
let vec = [|19;18;17;16;15;14;13;12;11;10;9;8;7;6;5;4;3;2;1;0|]  in
sort (<=) vec; vec;;



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
let vec = [|3;5;6;2;4;6;3;345;7;4;2;2;57;3;6;8;3;5;7;4;6;3;6;124;64;34;123|]  in
  sort (<=) vec; vec;;
 *)
