open Typedtree
open Types
open Asttypes
(*open Types

module L = Lightenv

include L*)



let col_lev = ref 0 (* amount of crap to collect *)
let ck_clev l = (l <= col_lev)

let is_function e =
  match e.exp_desc with
    | Texp_function(_, _) -> true
    | _ -> false 

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

let addm (typ, id) = 
  let id = Ident.name id in
  let _ = addt typ in
  let _ = addid id in 
    if (try String.sub id 0 5 = "__tmp" with Invalid_argument s -> false) then () else tymap := TM.add typ (IS.add id (findm typ)) !tymap 

let rec bound_idents pat = 
  let ptyp = pat.pat_type in
  match pat.pat_desc with 
    Tpat_var id -> addm (ptyp, id) 
    | Tpat_alias(p, id) -> bound_idents p; addm (ptyp, id)
    | Tpat_or(p, _, _) -> bound_idents p
    | d -> Typedtree.iter_pattern_desc bound_idents d

let all_consts () = CS.elements !intset 
let lookup_ids = findm 
let all_ids () = IS.elements !idset
let all_types () = TS.elements !tyset 

let rec visit_binding (pat, exp) = 
  let rec ve e =
    match (e.exp_type, e.exp_desc) with
(*| Texp_let of rec_flag * (pattern * expression) list * expression *)
  | (_, Texp_let (_, bl, e2)) ->
     List.iter visit_binding bl; ve e2  
  | (_, Texp_constant (Const_int (n))) ->
     addi n
  | (_, Texp_function([(pat, e)], _)) -> 
     bound_idents pat; ve e
  | (_, Texp_apply (e, el)) ->
     ve e; List.iter (function (Some(e), _) -> ve e | _ -> ()) el
(*| Texp_match of expression * (pattern * expression) list * partial
  x Texp_try of expression * (pattern * expression) list *)
  | (_, Texp_array (el))
  | (_, Texp_tuple (el)) 
  | (_, Texp_construct (_, el)) ->
     List.iter ve el
(*x Texp_variant of label * expression option*)
  | (_, Texp_record (el, None)) ->
     List.iter (fun (l, e) -> ve e) el
  | (_, Texp_assert (e))
  | (_, Texp_field (e, _)) ->
     ve e
  | (_, Texp_sequence (e1, e2))
  | (_, Texp_setfield (e1, _, e2)) ->
     ve e1; ve e2
  | (_, Texp_ifthenelse (e1, e2, e3)) ->
     ve e1; ve e2; (fun e3 -> match e3 with Some(e3) -> ve e3 | _ -> ()) e3
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
  let _ = if is_function exp then bound_idents pat else () in 
    ve exp
      
let rec visit_str sstr = 
  match sstr with
      Tstr_value (_, bl) :: srem ->
       List.iter visit_binding bl; visit_str sstr 
    | _ :: srem ->
       visit_str sstr
    | [] -> ()

let iter_bindings defs = 
  List.iter visit_binding defs


 

    








(*
(* Bookkeeping for qualifier generation *)


(*let constants : int list ref = ref []

let add_constant i =  
   try List.find (fun n -> n = i) !constants
      with Not_found -> constants := i::(!constants); -1

let labels : Types.type_expr list t ref = ref empty 

let add_label (lbl, t) =
  if mem lbl !labels then
    let ot = find lbl !labels in
    labels := (add lbl (t::ot) !labels)
  else
    labels := (add lbl [t] !labels)


let rec esc s oc nc = 
  let find_sp s = try String.index s oc with Not_found -> -1 in
  let next_sp = find_sp s in
  if next_sp = -1 then s else (String.fill s next_sp 1 nc; esc s oc nc)

let single_simple_qualif x fx i =
  let rels = ["<="; ">="; "!="] in
  let prels = ["_LE_"; "_GE_"; "_NE_"] in
  let ufx = String.uppercase fx in
  let _ = esc ufx ' ' '_' in 
  let _ = esc ufx '.' '_' in
  let _ = esc ufx '-' '_' in
  let _ = esc ufx '(' '_' in
  let _ = esc ufx ')' '_' in
  let ui = String.uppercase i in
  let _ = esc ui ' ' '_' in
  let _ = esc ui '.' '_' in
  let _ = esc ui '-' '_' in
  let _ = esc ui '(' '_' in
  let _ = esc ui ')' '_' in
  let mk_qualif op pop = Printf.sprintf "qualif Q%s%s%s(%s) : %s %s %s" ufx pop ui x fx op i in
  List.map2 mk_qualif rels prels
  
module StringSet = Set.Make(String)

let single_int_qualif path =
  let name = Path.name path in
  List.append (single_simple_qualif "_AA" "_AA" name)
   (List.append (single_simple_qualif "_AA" "Array.length _AA" name)
   (List.append (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" name)
      (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" name)))


let single_const_qualif i =
  let i = if i >= 0 then string_of_int i 
                    else Printf.sprintf "(0 - %d)" (abs i) in
  List.concat [(single_simple_qualif "_AA" "_AA" i);
              (single_simple_qualif "_AA" "Array.length _AA" i);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" i);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" i)]

let single_array_qualif path =
  let name = Path.name path in
  let array_length = "Array.length " ^ name in
  List.append (single_simple_qualif "_AA" "_AA" array_length)
              (single_simple_qualif "_AA" "Array.length _AA" array_length)

let single_bigarray_array2_qualif path =
  let name = Path.name path in
  let bigarray_array2_dim1 = "Bigarray.Array2.dim1 " ^ name in
  let bigarray_array2_dim2 = "Bigarray.Array2.dim2 " ^ name in
  List.concat [(single_simple_qualif "_AA" "_AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "_AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Array.length _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Array.length _AA" bigarray_array2_dim2);]
               
let dump_qualifs () =
  let consts = 0::1::!constants  in
  let lbls = !labels in
  let const_qualifs = List.concat (List.map single_const_qualif consts) in
  let lfilter p b ty = 
      let ty = Ctype.repr ty in
      b || 
      match ty with
        {desc = Tconstr(p', _, _)} ->
          Path.same p p'
        | t -> false
  in 
  let filter p path ts = if List.fold_left (lfilter p) false ts then Some path
                                                                else None
  in
  let int_lbls = mapfilter (filter Predef.path_int) lbls in
  let int_qualifs = List.concat (List.map single_int_qualif int_lbls) in
  let arr_lbls = mapfilter (filter Predef.path_array) lbls in
  let arr_qualifs = List.concat (List.map single_array_qualif arr_lbls) in
  let bigarray_lbls = mapfilter (filter (Builtins.ext_find_type_path "array2")) lbls in
  let bigarray_qualifs = List.concat (List.map single_bigarray_array2_qualif bigarray_lbls) in
  let multiset_quals = List.concat [bigarray_qualifs; arr_qualifs; int_qualifs; const_qualifs] in
    StringSet.elements (List.fold_left (fun s q -> StringSet.add q s) StringSet.empty multiset_quals) *) *)
