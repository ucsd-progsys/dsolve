(*
open Pi
open Db
*)

type str =
  | Str of string
  | Dummy

let can_read x = match x with
  | Str s -> 1
  | Dummy -> 0

let can_write x = match x with
  | Str s -> 2
  | Dummy -> 0

(*--- primBegin *)
let read file =
  assert (can_read file = 1 || can_write file = 2)

let delete file =
  assert (can_write file = 2)
(*--- primEnd *)

(* some sample files, one of them writable *)
(*--- filesBegin *)
let pwd    = Str "C:/etc/password"
let readme = Str "C:/public/README"
let tmp    = Str "C:/temp/tempfile"
let a      = assume (can_write tmp = 2)
(*--- filesEnd *)

(* some dynamic validation function
   "all files in the public directory are readable" *)
(*--- pfnBegin *)
let publicfile (f: str) =
  if f = Str "C:/public/README" then assume (can_read f = 1 && can_write f = 2)
  else assert false
(*--- pfnEnd *)

(*--- testbasicBegin *)
let test (x: unit) =
  delete tmp ; (* $\mbox{ok}$ *)
(*  delete pwd; (* $\mbox{type error}$ *) *)
  let v1 = read tmp in (* $\mbox{ok, using 1st logical rule}$ *)
(*  let v2 = read readme in (* $\mbox{type error}$ *) *)
    publicfile readme;
    let v3 = read readme in
      () (* $\mbox{ok}$ *)
(*--- testbasicEnd *)

(*
  
// some higher-order code
let rc file = fun (_:unit) -> read file

(*--- testhoBegin *)
let test_higher_order:unit =
  let reader: unit -> string = 
    (publicfile readme; (fun () -> read readme)) in
  let v4 = read readme in // $\mbox{type error}$ 
let v5 = reader () in () // $\mbox{ok}$
(*--- testhoEnd *)
  
// $\mbox{using dynamic ACLs in some database}$
type entry = 
  | Readable of string
  | Writable of string
  | Nothing

(*--- dbBegin *)
let acls: (string,entry) Db.t = Db.create()
let safe_read file = 
  match Db.select acls file with
    | Readable file -> read file 
    | Writable file -> read file 
    | _ -> failwith "unreadable"
let readable file = 
  match Db.select acls file with
    | Readable f when f = file -> ()
    | Writable f when f = file -> () 
    | _ -> failwith "unreadable"
(*--- dbEnd *)

(*--- dbexBegin *)
let test_acls:unit = 
  Db.insert acls tmp (Writable(tmp)); // $\mbox{ok}$
  Db.insert acls tmp (Readable(pwd)); // $\mbox{type error}$
  Db.insert acls pwd (Nothing); // $\mbox{ok}$
let v6 = safe_read pwd in // $\mbox{ok (but dynamically fails)}$
let v7 = readable tmp; read tmp in () // $\mbox{ok}$ 
(*--- dbexEnd *)

// merge 
type t = string

let rec fold_left f a xs = match xs with
  | x::xs -> fold_left f (f a x) xs
  | [] -> a
let append (a:string) (file:t) = (* a^ *) read file 
let merge sources = 
  let f: (string -> t -> string) -> string -> t list -> string = fold_left in
    f append "" sources

(*
// memoize 
type f = string -> unit

let memoize check = 
  let cache: (string,t) Db.t =  Db.create() in
  (fun x -> match Db.find cache x with
  | Some(y) -> ()
  | None -> check x; Db.insert cache x x):f 

let test_memo: f =  memoize readable
*)
      
*)
