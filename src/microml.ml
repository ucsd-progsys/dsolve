open Type


let main () =

    let lexbuf = Lexing.from_channel stdin in
      while true do
	Parser.query Lexer.token lexbuf;
      done

let _ = main ()
