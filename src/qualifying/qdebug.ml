open Qualifymod
open Btype
open Typedtree
open Parsetree
open Asttypes
open Types
open Format

let pprint_list sepstr pp =
  (fun ppf -> Oprint.print_list pp
     (fun ppf -> fprintf ppf "%s@;<1 2>" sepstr) ppf)

let rec print_typed_expression qmap ppf exp =
  let pprint = print_typed_expression qmap in
  let pprint_rec ppf = function
    | Recursive -> fprintf ppf "@ rec"
    | Nonrecursive -> fprintf ppf ""
    | _ -> assert false
  in
  let pprint_and ppf = function
    | [] -> fprintf ppf "@ "
    |  _ -> fprintf ppf "@ and@ " 
  in
  let rec pprint_binds ppf = function
    | ({pat_desc = Tpat_any}, e)::rem ->
      fprintf ppf "@[_@ =@;<1 2>%a%a%a@]" pprint e pprint_and rem pprint_binds rem
    | ({pat_desc = Tpat_var f}, e)::rem ->
      fprintf ppf "@[%s@ =@;<1 2>%a%a%a@]" (Ident.unique_name f) pprint e pprint_and rem pprint_binds rem
    | [] -> fprintf ppf ""
    | _ -> assert false
  in
  let rec pprint_exp ppf e =
    match e.exp_desc with
      | Texp_constant (Const_int n) ->
          fprintf ppf "%d" n
      | Texp_constant (Const_float f) ->
          fprintf ppf "%s" f
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
      | Texp_let (recf, binds, e2) ->
          fprintf ppf "@[let%a@ %a@;<1 0>in@;<1 2>%a@]"
            pprint_rec recf pprint_binds binds pprint e2
      (*| Texp_let (recf, binds, e2) ->
          fprintf ppf "@[let%a@ %s@ =@;<1 2>%a@;<1 0>in@;<1 2>%a@]"
            pprint_rec recf pprint_binds  *)
      | Texp_array es ->
          fprintf ppf "@[[|%a|]@]" (pprint_list ";" pprint) es
      | Texp_sequence(e1, e2) ->
          fprintf ppf "@[%a@]" (pprint_list ";" pprint) [e1; e2]
      | Texp_tuple(es) ->
          fprintf ppf "@[(%a)@]" (pprint_list "," pprint) es
      | Texp_assertfalse ->
          fprintf ppf "assert@ false"
      | _ -> assert false
  in
  let tytree = Printtyp.tree_of_type_scheme (repr exp.exp_type) in
  let qtree = Printqual.qualify_tree_of_type_scheme tytree (LocationMap.find exp.exp_loc qmap) in
    fprintf ppf "@[%a : %a@]" pprint_exp exp !Oprint.out_type qtree

let print_id id =
  String.concat "." (Longident.flatten id)

let rec pprint_expression ppf exp =
  let pprint_rec ppf = function
    | Recursive -> fprintf ppf "@ rec"
    | Nonrecursive -> fprintf ppf ""
    | _ -> assert false
  in
  let pprint_and ppf = function
    | [] -> fprintf ppf "@ "
    |  _ -> fprintf ppf "@ and@ " 
  in
  let rec pprint_binds ppf = function
    | ({ppat_desc = Ppat_any}, e)::rem ->
      fprintf ppf "@[_@ =@;<1 2>%a%a%a@]" pprint_expression e pprint_and rem pprint_binds rem
    | ({ppat_desc = Ppat_var f}, e)::rem ->
      fprintf ppf "@[%s@ =@;<1 2>%a%a%a@]" f pprint_expression e pprint_and rem pprint_binds rem
    | [] -> fprintf ppf ""
    | _ -> assert false
  in
  let rec pprint_exp ppf e =
    match e.pexp_desc with
      | Pexp_constant (Const_int n) ->
          fprintf ppf "%d" n
      | Pexp_constant (Const_float f) ->
          fprintf ppf "%s" f
      | Pexp_ident id ->
          fprintf ppf "%s" (print_id id)
      | Pexp_construct (tag, eopt, _) ->
          let tagstr = print_id tag in
            begin match eopt with
              | None -> fprintf ppf "%s" tagstr
              | Some e -> fprintf ppf "%s@ %a" tagstr pprint_expression exp
            end
      | Pexp_ifthenelse (e1, e2, Some e3) ->
          fprintf ppf "@[if@ %a@ then@;<1 4>%a@;<1 0>@[else@;<1 4>%a@]@]"
            pprint_expression e1 pprint_expression e2 pprint_expression e3
      | Pexp_function (_, _, [({ppat_desc = Ppat_var x}, e)]) ->
          fprintf ppf "@[fun@ %s@ ->@;<1 2>%a@]" x pprint_expression e
      | Pexp_apply (e1, exps) ->
          let pprint_arg ppf (_, e) =
            fprintf ppf "(%a)" pprint_expression e
          in fprintf ppf "@[(%a@;<1 2>%a)@]" pprint_expression e1 (pprint_list "" pprint_arg) exps
      | Pexp_let (recf, binds, e2) ->
          fprintf ppf "@[let%a@ %a@;<1 0>in@;<1 2>%a@]"
               pprint_rec recf pprint_binds binds pprint_expression e2
      (*| Pexp_let (recf, [({ppat_desc = Ppat_var f}, e1)], e2) ->
          let pprint_rec ppf = function
            | Recursive -> fprintf ppf "@ rec"
            | Nonrecursive -> fprintf ppf ""
            | _ -> assert false
          in fprintf ppf "@[let%a@ %s@ =@;<1 2>%a@;<1 0>in@;<1 2>%a@]"
               pprint_rec recf f pprint_expression e1 pprint_expression e2*)
      | Pexp_array es ->
          fprintf ppf "@[[|%a|]@]" (pprint_list ";" pprint_expression) es
      | Pexp_sequence(e1, e2) ->
          fprintf ppf "@[%a@]" (pprint_list ";" pprint_expression) [e1; e2]
      | Pexp_tuple(es) ->
          fprintf ppf "@[(%a)@]" (pprint_list "," pprint_expression) es
      | Pexp_assertfalse ->
          fprintf ppf "assert@ false"
      | _ -> assert false
  in fprintf ppf "@[%a@]" pprint_exp exp

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
