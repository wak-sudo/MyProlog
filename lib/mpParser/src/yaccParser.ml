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

open Parsing;;
let _ = parse_error;;
# 15 "./yaccParser.mly"

open Ast

let current_pos () =
  let start_p = symbol_start_pos () in
  let end_p   = symbol_end_pos () in
  { start  = start_p
  ; length = end_p.pos_cnum - start_p.pos_cnum
  }

let make data =
  { pos  = current_pos ()
  ; data = data
  }

# 36 "./yaccParser.ml"
let yytransl_const = [|
  260 (* BR_OPN *);
  261 (* BR_CLS *);
  262 (* ASTERISK *);
  263 (* COMMA *);
  264 (* DOT *);
  265 (* MINUS *);
  266 (* PLUS *);
  267 (* SLASH *);
  268 (* COLON_MINUS *);
  269 (* IS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* SYM *);
  259 (* NUM *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\005\000\005\000\006\000\007\000\007\000\
\008\000\008\000\009\000\009\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\013\000\013\000\014\000\
\014\000\015\000\001\000\002\000\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\001\000\
\003\000\001\000\003\000\001\000\002\000\001\000\003\000\001\000\
\001\000\001\000\004\000\001\000\003\000\002\000\004\000\000\000\
\002\000\001\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\024\000\000\000\000\000\029\000\000\000\000\000\016\000\006\000\
\017\000\000\000\003\000\002\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\025\000\027\000\
\000\000\013\000\000\000\000\000\001\000\000\000\000\000\004\000\
\005\000\000\000\028\000\022\000\000\000\015\000\000\000\021\000\
\007\000\009\000\011\000\000\000\019\000\023\000"

let yydgoto = "\003\000\
\004\000\013\000\030\000\014\000\034\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\023\000\005\000\006\000"

let yysindex = "\007\000\
\000\000\002\255\000\000\000\000\002\255\035\000\000\000\000\000\
\000\000\002\255\000\000\000\000\000\000\002\255\033\255\043\255\
\038\255\039\255\252\254\000\000\044\255\024\255\000\000\000\000\
\048\255\000\000\002\255\002\255\000\000\002\255\002\255\000\000\
\000\000\002\255\000\000\000\000\002\255\000\000\049\255\000\000\
\000\000\000\000\000\000\047\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\056\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\255\036\255\
\035\255\026\255\017\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\018\000\015\000\
\024\000\045\000\000\000\229\255\000\000\000\000\000\000"

let yytablesize = 59
let yytable = "\039\000\
\040\000\032\000\007\000\008\000\009\000\010\000\033\000\001\000\
\002\000\044\000\011\000\012\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\012\000\022\000\012\000\
\012\000\012\000\012\000\025\000\012\000\012\000\010\000\036\000\
\010\000\010\000\024\000\037\000\027\000\010\000\010\000\008\000\
\020\000\008\000\008\000\020\000\041\000\042\000\008\000\011\000\
\012\000\028\000\029\000\035\000\038\000\045\000\046\000\026\000\
\031\000\043\000\026\000"

let yycheck = "\027\000\
\028\000\006\001\001\001\002\001\003\001\004\001\011\001\001\000\
\002\000\037\000\009\001\010\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\005\001\005\000\007\001\
\008\001\009\001\010\001\010\000\012\001\013\001\005\001\008\001\
\007\001\008\001\000\000\012\001\004\001\012\001\013\001\005\001\
\005\001\007\001\008\001\008\001\030\000\031\000\012\001\009\001\
\010\001\007\001\013\001\008\001\005\001\005\001\008\001\000\000\
\018\000\034\000\014\000"

let yynames_const = "\
  BR_OPN\000\
  BR_CLS\000\
  ASTERISK\000\
  COMMA\000\
  DOT\000\
  MINUS\000\
  PLUS\000\
  SLASH\000\
  COLON_MINUS\000\
  IS\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  SYM\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "./yaccParser.mly"
     ( make "is" )
# 148 "./yaccParser.ml"
               : 'is_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "./yaccParser.mly"
        ( make "+" )
# 154 "./yaccParser.ml"
               : 'add_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "./yaccParser.mly"
        ( make "-" )
# 160 "./yaccParser.ml"
               : 'add_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "./yaccParser.mly"
           ( make "*" )
# 166 "./yaccParser.ml"
               : 'mult_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "./yaccParser.mly"
           ( make "/" )
# 172 "./yaccParser.ml"
               : 'mult_sym))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "./yaccParser.mly"
      ( make _1 )
# 179 "./yaccParser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_add) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'is_sym) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_add) in
    Obj.repr(
# 55 "./yaccParser.mly"
                           ( make (Sym(_2, [ _1; _3 ])) )
# 188 "./yaccParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_add) in
    Obj.repr(
# 56 "./yaccParser.mly"
           ( _1 )
# 195 "./yaccParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_mult) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'add_sym) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_add) in
    Obj.repr(
# 60 "./yaccParser.mly"
                             ( make (Sym(_2, [ _1; _3 ])) )
# 204 "./yaccParser.ml"
               : 'term_add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_mult) in
    Obj.repr(
# 61 "./yaccParser.mly"
            ( _1 )
# 211 "./yaccParser.ml"
               : 'term_add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_neg) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mult_sym) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_mult) in
    Obj.repr(
# 65 "./yaccParser.mly"
                              ( make (Sym(_2, [ _1; _3 ])) )
# 220 "./yaccParser.ml"
               : 'term_mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_neg) in
    Obj.repr(
# 66 "./yaccParser.mly"
           ( _1 )
# 227 "./yaccParser.ml"
               : 'term_mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'add_sym) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_neg) in
    Obj.repr(
# 70 "./yaccParser.mly"
                   ( make (Sym(_1, [ _2 ])) )
# 235 "./yaccParser.ml"
               : 'term_neg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_simple) in
    Obj.repr(
# 71 "./yaccParser.mly"
              ( _1 )
# 242 "./yaccParser.ml"
               : 'term_neg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 75 "./yaccParser.mly"
                     ( make (_2).data )
# 249 "./yaccParser.ml"
               : 'term_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "./yaccParser.mly"
                     ( make (Var  _1) )
# 256 "./yaccParser.ml"
               : 'term_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "./yaccParser.mly"
                     ( make (Num  _1) )
# 263 "./yaccParser.ml"
               : 'term_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 78 "./yaccParser.mly"
                     ( make (Atom _1) )
# 270 "./yaccParser.ml"
               : 'term_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 79 "./yaccParser.mly"
                                 ( make (Sym(_1, _3)) )
# 278 "./yaccParser.ml"
               : 'term_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 85 "./yaccParser.mly"
                       ( [ _1 ]   )
# 285 "./yaccParser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 86 "./yaccParser.mly"
                       ( _1 :: _3 )
# 293 "./yaccParser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 92 "./yaccParser.mly"
                                 ( make (Fact _1)      )
# 300 "./yaccParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 93 "./yaccParser.mly"
                                 ( make (Rule(_1, _3)) )
# 308 "./yaccParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "./yaccParser.mly"
                         ( []       )
# 314 "./yaccParser.ml"
               : 'clause_list_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause_list_rev) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 98 "./yaccParser.mly"
                         ( _2 :: _1 )
# 322 "./yaccParser.ml"
               : 'clause_list_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list_rev) in
    Obj.repr(
# 102 "./yaccParser.mly"
                  ( List.rev _1 )
# 329 "./yaccParser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause_list) in
    Obj.repr(
# 108 "./yaccParser.mly"
                  ( _1 )
# 336 "./yaccParser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 112 "./yaccParser.mly"
                ( _1 )
# 343 "./yaccParser.ml"
               : Ast.query))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry query *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
let query (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ast.query)
