
(** Get time/mem statistics for certain function applications.
    Adapted around CIL's ocamlutil "Stats" to measure wall-clock time
    instead of user-mode time. *)

let doTime = ref false

type t = { name : string;
           mutable time : float; (* In seconds *)
           mutable sub  : t list}

                                        (* Create the top level *)
let top = { name = "TOTAL";
            time = 0.0;
            sub  = []; }

                                        (* The stack of current path through 
                                         * the hierarchy. The first is the 
                                         * leaf. *)
let current : t list ref = ref [top]

let reset = 
  top.sub <- [];
  top.time = 0.0

(* TODO: make this work w/ my Logging module *)
  
let print chn msg =
  if !doTime then begin
    (* Total up *)
    top.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 top.sub;
    let rec prTree ind node = 
      (Printf.fprintf chn "%s%-20s          %6.3f s\n" 
         (String.make ind ' ') node.name node.time);
      
      List.iter (prTree (ind + 2)) (List.rev node.sub)
    in
    Printf.fprintf chn "%s" msg; 
    List.iter (prTree 0) [ top ];
    let gc = Gc.quick_stat () in 
    let printM (w: float) : string = 
      Printf.sprintf "%.2fMb" (w *. 4.0 /. 1000000.0)
    in
    Printf.fprintf chn 
      "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
      (printM (gc.Gc.minor_words +. gc.Gc.major_words 
               -. gc.Gc.promoted_words))
      (printM (float_of_int gc.Gc.top_heap_words))
      (printM gc.Gc.minor_words)
      (printM gc.Gc.major_words)
      (printM gc.Gc.promoted_words)
      gc.Gc.minor_collections
      gc.Gc.major_collections
      gc.Gc.compactions;
  end
        
  

(* Get the current time from user process's perspective, in seconds *)
let getUserTime () : float = 
  (Unix.times ()).Unix.tms_utime

(* Get the current wall-clock time *)
let getWCTime = Unix.gettimeofday


let repeatTime getTime limit str f arg = 
  (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | _ -> assert false in
    let rec loop = function
        h :: _ when h.name = str -> h
      | _ :: rest -> loop rest
      | [] -> 
          let nw = {name = str; time = 0.0; sub = []} in
          curr.sub <- nw :: curr.sub;
          nw
    in
    loop curr.sub
  in
  let oldcurrent = !current in
  current := stat :: oldcurrent;
  let start = getTime () in
  let rec repeatf count = 
    let res   = f arg in
    let diff = getTime () -. start in
    if diff < limit then
      repeatf (count + 1)
    else begin
      stat.time <- stat.time +. (diff /. float(count));
      current := oldcurrent;                (* Pop the current stat *)
      res                                   (* Return the function result *)
    end
  in
  repeatf 1


let time str f arg =
  if !doTime then begin
    try 
      repeatTime getWCTime 0.0 str f arg
    with e ->
      prerr_string ("Uncaught exc. in Mystats (shouldn't escape like this!): " ^
                      (Printexc.to_string e) ^ "\n") ;
      prerr_string ("Currently timing " ^ str ^ "\n");
      print stdout "STATS:\n";
      exit 1
  end 
  else
    f arg


let lastTime = ref 0.0
let timethis (f: 'a -> 'b) (arg: 'a) : 'b = 
  let start = getUserTime () in
  let res = f arg in 
  lastTime := getUserTime () -. start; 
  res
    

(************************************************************)

module type IndexOps = sig

  type t (* type of index *)

  val hash : t -> int

  val equal : t -> t -> bool

  val to_string : t -> string

  val getTime : unit -> float (* slipped this in there... *)

(* TODO: make output channel a parameter? *)

  val prefix : string

end

module IndexedTimer (I:IndexOps) = struct

  module HI = Hashtbl.Make(I)

  let times = ref (HI.create 4)

  let updateTime idx moreTime =
    let oldTime = try HI.find !times idx with Not_found -> 0.0 in
    HI.replace !times idx (oldTime +. moreTime)

  let time idx foo arg =
    let start = I.getTime () in
    let res = foo arg in
    let newTime = I.getTime () -. start in
    updateTime idx newTime;
    res

  let printTime idx time =
    print_string (I.prefix ^ I.to_string idx ^ 
                    " : " ^ string_of_float time ^ "\n")

  let printTimes () =
    HI.iter printTime !times

  let reset () = 
    times := HI.create 4
      
end

