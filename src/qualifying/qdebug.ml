open Qualifymod
open Btype
open Typedtree
open Asttypes
open Types
open Format

let rec print_typed_expression qmap ppf exp =
  let pprint = print_typed_expression qmap in
  let pprint_list sepstr pp =
    (fun ppf -> Oprint.print_list pp
       (fun ppf -> fprintf ppf "%s@;<1 2>" sepstr) ppf)
  in
  let rec pprint_exp ppf e =
    match e.exp_desc with
      | Texp_constant (Const_int n) ->
          fprintf ppf "%d" n
      | Texp_ident (path, _) ->
          fprintf ppf "%s" (Path.name path)
      | Texp_construct ({cstr_tag = Cstr_constant n}, []) ->
          fprintf ppf "%d" n
      | Texp_ifthenelse (e1, e2, Some e3) ->
          fprintf ppf "@[if@ %a@ then@;<1 4>%a@;<1 0>@[else@;<1 4>%a@]@]"
            pprint e1 pprint e2 pprint e3
      | Texp_function ([({pat_desc = Tpat_var x}, e)], _) ->
          fprintf ppf "@[fun@ %s@ ->@;<1 2>%a@]"
            (Ident.unique_name x) pprint e
      | Texp_apply (e1, exps) ->
          let pprint_arg ppf = function
            | (Some e, _) -> fprintf ppf "(%a)" pprint e
            | (None, _) -> fprintf ppf "(none)"
          in fprintf ppf "@[(%a@;<1 2>%a)@]" pprint e1 (pprint_list "" pprint_arg) exps
      | Texp_let (recf, [({pat_desc = Tpat_var f}, e1)], e2) ->
          let pprint_rec ppf = function
            | Recursive -> fprintf ppf "@ rec"
            | Nonrecursive -> fprintf ppf ""
            | _ -> assert false
          in fprintf ppf "@[let%a@ %s@ =@;<1 2>%a@;<1 0>in@;<1 2>%a@]"
               pprint_rec recf (Ident.unique_name f) pprint e1 pprint e2
      | Texp_array es ->
          fprintf ppf "@[[|%a|]@]" (pprint_list ";" pprint) es
      | _ -> assert false
  in
  let tytree = Printtyp.tree_of_type_scheme (repr exp.exp_type) in
  let qtree = Printqual.qualify_tree_of_type_scheme tytree (LocationMap.find exp.exp_loc qmap) in
    fprintf ppf "@[%a : %a@]" pprint_exp exp !Oprint.out_type qtree

let rec dump_qualified_structure ppf framemap = function
  | [] -> ()
  | Tstr_eval exp :: srem ->
      fprintf ppf "@[%a@]@." (print_typed_expression framemap) exp;
      dump_qualified_structure ppf framemap srem
  | Tstr_qualifier (name, (valu, pred)) :: srem ->
      fprintf ppf "@[qualdef@ %s@ %s@;<1 2>%a@]@." (Ident.name name)
        (Ident.unique_name valu) Predicate.pprint pred;
      dump_qualified_structure ppf framemap srem
  | _ -> assert false
