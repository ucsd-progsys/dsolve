open Type


let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Parser.query Lexer.token lexbuf
      done
  with End_of_file ->
    ()

let _ = main ()
