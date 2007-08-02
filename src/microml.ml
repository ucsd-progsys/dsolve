exception PPerror

let main () =
  let lb = Lexing.from_channel stdin in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  while true do
    try
      Lexing.flush_input lb;
      Location.reset();
      let phr = try Parse.toplevel_phrase lb with Exit -> raise PPerror in
      ignore(Printast.top_phrase Format.std_formatter phr)
    with
    | End_of_file -> exit 0
    | PPerror -> ()
  done

let _ = main ()
