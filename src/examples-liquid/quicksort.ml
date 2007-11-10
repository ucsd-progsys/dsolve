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

let fwhile b body =
  let rec whiledo _n =
    if b () then (body (); whiledo ()) else ()
  in whiledo ()
in

let incr i =
  i := !i + 1
in
let decr i =
  i := !i - 1
in

(* There is a known performance bug in the code below.  If you find
   it, don't bother reporting it.  You're not supposed to use this
   module anyway. *)
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
      let i = ref (lo + 1) in
      let j = ref (hi - 1) in
      if not (cmp pivot (Array.get arr hi))
         || not (cmp (Array.get arr lo) pivot)
      then assert false else ();
      let b1 _n = !i < !j in
      let bod1 _n = 
        let b2 _n = cmp pivot (Array.get arr !i) in
        let bod2 _n = incr i in
          fwhile b2 bod2;
        let b3 _n = cmp (Array.get arr !j) pivot in
        let bod3 _n = decr j in
          fwhile b3 bod3; 
        if !i < !j then swap arr !i !j else ();
        incr i; decr j
      in fwhile b1 bod1;
      (* Recursion on smaller half, tail-call on larger half *)
      if !j - lo <= hi - !i then begin
        qsort lo !j; qsort !i hi
      end else begin
        qsort !i hi; qsort lo !j
      end
    end 
    else () 
  in qsort 0 (Array.length arr - 1);
  (* Finish sorting by insertion sort *)
  let forbod i = 
    let val_i = (Array.get arr i) in
    if not (cmp (Array.get arr (i - 1)) val_i) then begin
      Array.set arr i (Array.get arr (i - 1));
      let j = ref (i - 1) in
      let _ = (fun x -> x) !j in
      let _ = (fun x -> x) i in
      let rec wbod _n =
        let dj = !j in
          if dj >= 1 then if not (cmp (Array.get arr (dj - 1)) val_i) then
            (Array.set arr dj (Array.get arr (dj-1));
            j := dj - 1; wbod ()) else () else ()
      in wbod ();
      Array.set arr !j val_i
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
