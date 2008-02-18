open Typedtree
open Types
open Asttypes


let col_lev = ref 100 (* amount of crap to collect *)
let ck_clev l = (l <= !col_lev)

let is_function e =
  match e.exp_desc with
    | Texp_function(_, _) -> true
    | _ -> false 

module C = Common

module CTy = 
struct
  type t = Types.type_expr
  let compare = compare
end

module TM = Map.Make(CTy)
module TS = Set.Make(CTy)
module IS = Set.Make(String)
module CS = Set.Make(struct
                       type t = int
                       let compare = compare
                     end)

(* tymap: map from shapes to all idents of that shape *)
(* idset: set of all idents *)
(* intset: set of all int constants. ignored or set {0,1} if lquals set? *)

let tymap = ref TM.empty   
let tyset = ref TS.empty
let idset = ref IS.empty
let intset = ref CS.empty

let addi n =
  intset := CS.add n !intset 

let addt t =
  tyset := TS.add t !tyset

let addid i =
  idset := IS.add i !idset

let findm ty = try TM.find ty !tymap with Not_found -> IS.empty

let addm n (typ, id) = 
(*  let _ = C.cprintf C.ol_always "@[%i@ %i@ %i@ %i@\n@]" 
                                (C.map_cnt TM.fold !tymap)
                                (C.set_cnt TS.elements !tyset)
                                (C.set_cnt IS.elements !idset)
                                (C.set_cnt CS.elements !intset)
  in*)
  let id = Ident.name id in
  let tmp = try (String.sub id 0 5) = "__tmp" with Invalid_argument s -> false in
  let tmp = tmp && ck_clev n in
  let _ = if tmp then () else addt typ in
  let _ = if tmp then () else addid id in 
    if tmp then () else tymap := TM.add typ (IS.add id (findm typ)) !tymap 

let bound_idents n pat = 
  let rec bound_idents_rec pat =
    let ptyp = pat.pat_type in
    match pat.pat_desc with 
      Tpat_var id -> addm n (ptyp, id) 
      | Tpat_alias(p, id) -> bound_idents_rec p; addm n (ptyp, id)
      | Tpat_or(p, _, _) -> bound_idents_rec p
      | d -> Typedtree.iter_pattern_desc bound_idents_rec d 
  in bound_idents_rec pat

let all_consts () = CS.elements !intset 
let lookup_ids = findm 
let all_ids () = IS.elements !idset
let all_types () = TS.elements !tyset 

let rec visit_binding n (pat, exp) = 
  let rec ve n e =
    match (e.exp_type, e.exp_desc) with
  | (_, Texp_let (_, bl, e2)) ->
     List.iter (visit_binding n) bl; ve n e2  
  | (_, Texp_constant (Const_int (i))) ->
     addi i
  | (_, Texp_function(pl, _)) -> 
     List.iter (fun (pat, e) -> bound_idents n pat; ve n e) pl
  | (_, Texp_apply (e, el)) ->
     ve n e; List.iter (function (Some(e), _) -> ve n e | _ -> ()) el
(*| Texp_match of expression * (pattern * expression) list * partial
  x Texp_try of expression * (pattern * expression) list *)
  | (_, Texp_array (el))
  | (_, Texp_tuple (el)) 
  | (_, Texp_construct (_, el)) ->
     List.iter (ve n) el
(*x Texp_variant of label * expression option*)
  | (_, Texp_record (el, None)) ->
     List.iter (fun (l, e) -> ve n e) el
  | (_, Texp_assert (e))
  | (_, Texp_field (e, _)) ->
     ve n e
  | (_, Texp_sequence (e1, e2))
  | (_, Texp_setfield (e1, _, e2)) ->
     ve n e1; ve n e2
  | (_, Texp_ifthenelse (e1, e2, e3)) ->
     ve n e1; ve n e2; (fun e3 -> match e3 with Some(e3) -> ve n e3 | _ -> ()) e3
(*x Texp_while of expression * expression
  x Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  x Texp_when of expression * expression
  x Texp_send of expression * meth
  x Texp_new of Path.t * class_declaration
  x Texp_instvar of Path.t * Path.t
  x Texp_setinstvar of Path.t * Path.t * expression
  x Texp_override of Path.t * (Path.t * expression) list
  x Texp_letmodule of Ident.t * module_expr * expression 
  x Texp_lazy of expression
  x Texp_object of class_structure * class_signature * string list *)
  | (_, Texp_assertfalse)
  | (_, Texp_ident (_, _)) ->
      ()
  | _ ->
      assert false
  in 
    if is_function exp then (bound_idents n pat; ve (n+1) exp)
                       else ve n exp 


let rec visit_str sstr = 
  match sstr with
      Tstr_value (_, bl) :: srem ->
       addi 0; addi 1; List.iter (visit_binding 0) bl; visit_str sstr 
    | _ :: srem ->
       visit_str sstr
    | [] -> ()

let iter_bindings defs = 
  List.iter (visit_binding 0) defs


