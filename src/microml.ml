open Expr
open Frame
open Parsetree
open Infer


let type_expression exp =
  let framemap = infer_frames exp Env.empty in
  let annotate e = pprint_frame (ExpMap.find e framemap) in
    pprint_annotated_expression annotate 0 exp


exception PPerror

let eval_structure_item si =
  match si.pstr_desc with
      Pstr_eval (e) ->
	type_expression e
    | _ -> raise PPerror


let main () =
  let lb = Lexing.from_channel stdin in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  while true do
    try
      Lexing.flush_input lb;
      Location.reset();
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
