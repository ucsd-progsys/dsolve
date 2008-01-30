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

let print_id id =
  String.concat "." (Longident.flatten id)

let rec pprint_expression ppf exp =
  let pprint_binding ppf (lid, exp) =
    fprintf ppf "@[%s =@;<1 2>%a@]" (print_id lid) pprint_expression exp
  in
  let rec pprint_exp ppf e =
    match e.pexp_desc with
      | Pexp_constraint (_, _, _) ->
          fprintf ppf ": ty"
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
      | Pexp_function (_, _, [(pat, e)]) ->
          fprintf ppf "@[fun %a ->@;<1 2>%a@]" pprint_pattern pat pprint_expression e
      | Pexp_apply (e1, exps) ->
          let pprint_arg ppf (_, e) =
            fprintf ppf "(%a)" pprint_expression e
          in fprintf ppf "@[(%a@;<1 2>%a)@]" pprint_expression e1 (pprint_list "" pprint_arg) exps
      | Pexp_let (recf, binds, e2) ->
          fprintf ppf "@[let%a@ %a@;<1 0>in@;<1 2>%a@]"
               pprint_rec recf pprint_binds binds pprint_expression e2
      | Pexp_array es ->
          fprintf ppf "@[[|%a|]@]" (pprint_list ";" pprint_expression) es
      | Pexp_sequence(e1, e2) ->
          fprintf ppf "@[%a@]" (pprint_list ";" pprint_expression) [e1; e2]
      | Pexp_tuple(es) ->
          fprintf ppf "@[(%a)@]" (pprint_list "," pprint_expression) es
      | Pexp_record (bindings, None) ->
          fprintf ppf "@[{%a}@]" (pprint_list ";" pprint_binding) bindings
      | Pexp_field (e, lid) ->
          fprintf ppf "@[%a.%s@]" pprint_expression e (print_id lid)
      | Pexp_assertfalse ->
          fprintf ppf "assert@ false"
      | _ -> assert false
  in fprintf ppf "@[%a@]" pprint_exp exp
and pprint_rec ppf = function
  | Recursive -> fprintf ppf "@ rec"
  | Nonrecursive -> fprintf ppf ""
  | _ -> assert false
and pprint_pat_list ppf ls =
  match ls with
    t::[] -> fprintf ppf "%a" pprint_pattern t
  | t::rem -> fprintf ppf "%a,@ %a" pprint_pattern t pprint_pat_list rem
  | [] -> fprintf ppf ""
and pprint_pattern ppf pat =
  match pat.ppat_desc with
    Ppat_any -> fprintf ppf "_"
  | Ppat_var x -> fprintf ppf "%s" x
  | Ppat_tuple ts -> fprintf ppf "(%a)" pprint_pat_list ts
  | Ppat_constraint (p, _) -> pprint_pattern ppf p
  | _ -> assert false
and pprint_and ppf = function
  | [] -> fprintf ppf "@ "
  |  _ -> fprintf ppf "@ and@ " 
and pprint_binds ppf = function
  | (pat, e)::rem ->
    fprintf ppf "@[%a@ =@;<1 2>%a%a%a@]" pprint_pattern pat pprint_expression e pprint_and rem pprint_binds rem
  | [] -> fprintf ppf ""

let rec pprint_structure ppf str = 
  match str.pstr_desc with
  | Pstr_eval(exp) ->
      pprint_expression ppf exp
  | Pstr_value(recursive, pl) ->
      fprintf ppf "@[let%a@ %a@]" pprint_rec recursive pprint_binds pl
  | _ -> assert false
