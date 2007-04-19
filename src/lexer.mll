{

  open Parser

}


let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = (lowercase | uppercase)
let sign = ['+' '-']
let digit = ['0'-'9']


rule token = parse
  | blank +
      { token lexbuf }
  | "and"
      { AND }
  | "->"
      { ARROW }
  | "!"
      { BANG }
  | "bool"
      { BOOL }
  | "bot"
      { BOTTOM }
  | ":"
      { COLON }
  | "."
      { DOT }
  | "else"
      { ELSE }
  | newline
      { EOL }
  | "="
      { EQUAL }
  | "false"
      { FALSE }
  | "fun"
      { FUN }
  | "if"
      { IF }
  | "in"
      { IN }
  | "int"
      { INT }
  | sign? digit+ as intstring
      {	INTLITERAL(int_of_string intstring) }
  | "|"
      { JOIN }
  | "{"
      { LCURLY }
  | "<"
      { LESS }
  | "<="
      { LESSEQ }
  | "let"
      { LET }
  | "("
      { LPAREN }
  | "["
      { LSQUARE }
  | "&"
      { MEET }
  | "-"
      { MINUS }
  | "!="
      { NEQUAL }
  | "not"
      { NOT }
  | "or"
      { OR }
  | "+"
      { PLUS }
  | "pred"
      { PRED }
  | uppercase +
      { QLITERAL (Lexing.lexeme lexbuf) }
  | "?"
      { QUESTION }
  | "'" uppercase
      { let s = Lexing.lexeme lexbuf in
	let name = String.sub s 1 (String.length s - 1) in
	  QVAR name }
  | "}"
      { RCURLY }
  | ")"
      { RPAREN }
  | "]"
      { RSQUARE }
  | "then"
      { THEN }
  | "*"
      { TIMES }
  | "top"
      { TOP }
  | "true"
      { TRUE }
  | "'" lowercase
      { let s = Lexing.lexeme lexbuf in
	let name = String.sub s 1 (String.length s - 1) in
	  TVAR name }
  | lowercase + as name
      { VAR name }
  | eof
      { raise End_of_file }
  | _
      { token lexbuf }
