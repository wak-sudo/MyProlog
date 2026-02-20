type token =
  | VAR of (string)
  | SYM of (string)
  | NUM of (int)
  | BR_OPN
  | BR_CLS
  | ASTERISK
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SLASH
  | COLON_MINUS
  | IS
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
val query :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.query
