(* DSOLVE -dontminemlq *)

(*
** by: Dave Edelblute, edelblut@cod.nosc.mil, 05 Jan 1993
** Modified: R. Mayer to work with hist benchmark routines.
** Translated from C to de Caml: Hongwei Xi, 07 Nov 1998
*)

let pi = 3.14 

let two_pi = 2.0 *. pi  

let ffor s d body = 
    let rec loop i =
        let i' = i + 1 in 
        if i <= d then (body i; loop i') else () 
    in loop s

let fft px py n = (* n must be a power of 2! *)
  let rec loop n2 n4 =
    if n2 <= 2 then () else (* the case n2 = 2 is treated below *)
      let e = two_pi /. (float_of_int n2) in
      let e3 = 3.0 *. e in
      let a = ref 0.0 in
      let a3 = ref 0.0 in
      
      for j = 1 to n4 do
        let cc1 = cos !a in
        let ss1 = sin !a in
        let cc3 = cos !a3 in
        let ss3 = sin !a3 in
        let _   = a  := !a +. e in 
        let _   = a3 := !a3 +. e3 in 
        let rec loop1 i0 i1 i2 i3 id =
          if i3 > n then () else (* out_of_bounds *)
            let g_px_i0 = Array.get px i0 in
            let g_px_i2 = Array.get px i2 in
            let r1      = g_px_i0 -. g_px_i2 in
            let r1'     = g_px_i0 +. g_px_i2 in
            let _       = Array.set px i0 r1' in
        
            let g_px_i1 = Array.get px i1 in
            let g_px_i3 = Array.get px i3 in
            let r2      = g_px_i1 -. g_px_i3 in
            let r2'     = g_px_i1 +. g_px_i3 in
            let _       = Array.set px i1 r2' in
        
            let g_py_i0 = Array.get py i0 in
            let g_py_i2 = Array.get py i2 in
            let s1      = g_py_i0 -. g_py_i2 in
            let s1'     = g_py_i0 +. g_py_i2 in
            let _       = Array.set py i0 s1' in
        
            let g_py_i1 = Array.get py i1 in
            let g_py_i3 = Array.get py i3 in
            let s2      = g_py_i1 -. g_py_i3 in
            let s2'     = g_py_i1 +. g_py_i3 in
            let _       = Array.set py i1 s2' in
        
            let s3      = r1 -. s2 in 
            let r1      = r1 +. s2 in
            let s2      = r2 -. s1 in
            let r2      = r2 +. s1 in
        
            Array.set px i2 (r1 *. cc1 -. s2 *. ss1); 
            Array.set py i2 ((-. s2) *. cc1 -. r1 *. ss1); 
            Array.set px i3 (s3 *. cc3 +. r2 *. ss3); 
            Array.set py i3 (r2 *. cc3 -. s3 *. ss3);
            loop1 (i0 + id) (i1 + id) (i2 + id) (i3 + id) id
        in
      
        let rec loop2 is id =
          if is >= n then () else begin
            let i1 = is + n4 in
            let i2 = i1 + n4 in
            let i3 = i2 + n4 in
            loop1 is i1 i2 i3 id;
            loop2 (2 * id - n2 + j) (4 * id)
          end
        in loop2 j (2 * n2)
      done; 
      loop (n2 / 2) (n4 / 2) 
  in loop n (n / 4);
    
  let rec loop1 i0 i1 id =
    if i1 > n then () else
      let r1 = Array.get px i0 in
      let _  = Array.set px i0 (r1 +. (Array.get px i1)) in
      let _  = Array.set px i1 (r1 -. (Array.get px i1)) in
      let r1 = Array.get py i0 in
      let _  = Array.set py i0 (r1 +. (Array.get py i1)) in
      let _  = Array.set py i1 (r1 -. (Array.get py i1)) in
      loop1 (i0 + id) (i1 + id) id
  in
  let rec loop2 is id =
    if is >= n then () else begin
      loop1 is (is + 1) id;
      loop2 (2 * id - 1) (4 * id)
    end
  in
  loop2 1 4;

    
  let rec loop1 j k =
    if k >= j then 
      j + k 
    else 
      loop1 (j - k) (k / 2)
  in
    
  let rec loop2 i j =
    if i >= n then () else begin
      if i >= j then () else begin
        let xt = Array.get px j in 
        let _  = Array.set px j (Array.get px i) in
        let _  = Array.set px i (xt) in
        let xt = Array.get py j in 
        let _  = Array.set py j (Array.get py i) in
        let _  = Array.set py i (xt) in ()
      end;
      loop2 (i + 1) (loop1 j (n / 2))
    end
  in loop2 1 1; n

let fabs r = if r > 0.0 then r else (0.0 -. r)
                                     
let ffttest np =
  let enp = float_of_int np in
  let n2  = np / 2 in
  let npm = n2 - 1 in
  let pxr = Array.make (np (* + 1 BUG *) ) 0.0 in
  let pxi = Array.make (np+1) 0.0 in
  let t   = pi /. enp in
  let _   = Array.set pxr 1 ((enp -. 1.0) *. 0.5) in
  let _   = Array.set pxi 1 (0.0) in
  let _   = Array.set pxr (n2+1) ((-. (1.0 *. 0.5))) in
  let _   = Array.set pxi (n2+1) (0.0) in
  
  for i = 1 to npm do 
    let j = np - i in
    let none_ = Array.set pxr (i+1) (-. (1.0 *. 0.5)) in
    let none_ = Array.set pxr (j+1) (-. (1.0 *. 0.5)) in
    let z = t *. (float_of_int i) in
    let y = (cos z /. sin z) *. 0.5 in
    Array.set pxi (i+1) (-. y); 
    Array.set pxi (j+1) (y)
  done; 
  ignore (fft pxr pxi np);
  let rec loop i zr zi kr ki =
    if i >= np then (zr, zi) else
      let a  = fabs((Array.get pxr (i+1)) -. (float_of_int i)) in
      let b  = zr < a in
      let zr = if b then a else zr in
      let kr = if b then i else kr in
      let a  = fabs(Array.get pxi (i+1)) in
      let b  = zi < a in
      let zi = if b then a else zi in
      let ki = if b then i else ki in
      loop (i+1) zr zi kr ki
  in
  let zz = loop 0 0.0 0.0 0 0 in
  let zr = fst zz in
  let zi = snd zz in
  let zm = if fabs zr < fabs zi then zi else zr
  in print_float zm; print_newline ()

let rec loop_np i np =
  if i <= 16 then begin 
    ignore (ffttest np); 
    loop_np (i + 1) (np * 2) 
  end else ()

let driver = loop_np 4 16 
