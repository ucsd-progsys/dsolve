open Expr
open Frame
open Type
open Predicate
open Parsetree
open Infer


let qualifier_env = ref Env.empty


let type_expression exp =
  let framemap = infer_frames exp !qualifier_env in
  let annotate e = pprint_frame (ExpMap.find e framemap) in
    pprint_annotated_expression annotate 0 exp


let add_qualifier q {pqual_desc = Pqual(x, pred)} =
  let p = parse_predicate pred in
  let qual = PredOver(x, p) in
    qualifier_env := Env.add q qual !qualifier_env;
    Printf.sprintf "qualifier %s(%s): %s\n" q x (pprint_predicate p)


exception PPerror

let eval_structure_item si =
  match si.pstr_desc with
      Pstr_eval (e) ->
	type_expression e
    | Pstr_qual (q, qd) ->
	add_qualifier q qd
    | _ -> raise PPerror


let main () =
  let lb = Lexing.from_channel stdin in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  while true do
    try
      let phr = try Parse.toplevel_phrase lb with Exit -> raise PPerror in
	match phr with
	    Ptop_def (is) ->
	      Printf.printf "%s\n" (Misc.join (List.map eval_structure_item is) "\n")
	  | _ -> raise PPerror
    with
    | End_of_file -> exit 0
    | PPerror -> ()
  done

let _ = main ()
