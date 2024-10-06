{
  open Parser
  exception Eof
}

rule token = parse
    [' ' '\t' '\n']                     { token lexbuf }
    | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as str
                                        { match str with
                                          | "fn" -> FN
                                          | "return" -> RETURN
                                          | "write" -> WRITE
                                          | _ -> IDENTIFIER str }
  | ['0'-'9']+ as lxm                   { INT (int_of_string lxm) }
  | ['0'-'9']+'.'['0'-'9']* as lxm      { DOUBLE (float_of_string lxm) }
  | "="                                 { EQUAL }
  | "=="                                { EQ }
  | "!="                                { NE }
  | "<"                                 { LT }
  | "<="                                { LE }
  | ">"                                 { GT }
  | ">="                                { GE }
  | "&&"                                { AND }
  | "||"                                { OR }
  | "+"                                 { PLUS }
  | "-"                                 { MINUS }
  | "*"                                 { MUL }
  | "/"                                 { DIV }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { LBRACE }
  | '}'                                 { RBRACE }
  | ','                                 { COMMA }
  | ';'                                 { SEMICOLON }
  | eof                                 { EOF }

