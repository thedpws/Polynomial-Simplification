{
    open Parser
    exception Eof
}

rule token = parse
  | [' ' '\t']          { token lexbuf }
  | ['\n']              { EOL }
  | [a-zA-Z_][a-zA-Z0-9_]* as id { IDENTIFIER (id) }
  | ['0'-'9']+ as lxm   { NUM (int_of_string lxm) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | '^'                 { POW }
  | '='                 { ASSIGN }
  | "=="                { EQ }
  | "!="                { NEQ }
  | '<'                 { LT }
  | "<="                { LTEQ }
  | '>'                 { GT }
  | ">="                { GTEQ }
  | ['a'-'z'] as lxm    { VAR (lxm) }
  | eof                 { raise Eof }