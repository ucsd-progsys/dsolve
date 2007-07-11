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
  | newline +
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
  | "::"
      { COLONCOLON }
  | ","
      { COMMA }
  | "#" (alpha | blank) *
      { token lexbuf }
  | "."
      { DOT }
  | "else"
      { ELSE }
  | "[]"
      { EMPTYSQBRACKETS }
  | ";;"
      { EOL }
  | "="
      { EQUAL }
  | "false"
      { FALSE }
  | "fix"
      { FIX }
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
  | "letrec"
      { LETREC }
  | "("
      { LPAREN }
  | "["
      { LSQUARE }
  | "match"
      { MATCH }
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
  | uppercase +
      { QLITERAL (Lexing.lexeme lexbuf) }
  | "qual"
      { QUAL }
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
  | uppercase alpha +
      { TAG (Lexing.lexeme lexbuf) }
  | "then"
      { THEN }
  | "*"
      { TIMES }
  | "top"
      { TOP }
  | "true"
      { TRUE }
  | "type"
      { TYPE }
  | "'" lowercase
      { let s = Lexing.lexeme lexbuf in
	let name = String.sub s 1 (String.length s - 1) in
	  TVAR name }
  | "with"
      { WITH }
  | lowercase + as name
      { VAR name }
  | eof
      { raise End_of_file }
  | _
      { token lexbuf }
