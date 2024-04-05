{
    open Parser
}

let white = [' ' '\t']+
let newline = '\n' | '\r'
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z' '/' '.' '_']
let id = letter+


rule read =
  parse
  | newline {Lexing.new_line lexbuf; read lexbuf}
  | white { read lexbuf }
  | ";" { SEMICOLON }
  | ";;" { DOUBLESEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "letcond" { CONDLET }
  | "strat" { STRAT }
  | "=" { EQUALS }
  | ":=" { BEQUALS }
  | "do" { APPLY }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "}" { RBRACE }
  | "{" { LBRACE }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "?" { CHOICE }
  | "use" { USE }
  | "any" { ANY }
  | "while" { WHILE }
  | "appcond" { APPCOND }
  | "using" { USING }
  | "for" { REPEAT }
  | "upto" { UPTO }
  | "ID" { IDENTITY }
  | "FAIL" { FAIL }
  | "check" { CHECK }
  | "in" { IN }
  | "+" { PARAM }
  | "-" { CTX }
  | "!" { BANG }
  | "," { COMMA }
  | "#" { read_single_line_comment lexbuf }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }

and read_single_line_comment = parse
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }