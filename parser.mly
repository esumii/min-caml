%{
(* parser�����Ѥ����ѿ����ؿ������ʤɤ����� *)
let addtyp x = (x, Type.gentyp ())
let parse_error s = print_endline s
open Syntax
let perr_handling pos =
  let open Lexing in 
    Format.eprintf "\x1b[1mline %d, column %d\x1b[0m: @.\x1b[1m\x1b[31mError\x1b[39m\x1b[0m: Syntax error@."
    pos.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
    (* (Lexing.lexeme lexbuf) *)
    
%}

/* (* ������ɽ���ǡ����������� (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <Id.pos> NOT
%token <Id.pos> MINUS
%token <Id.pos> PLUS
%token <Id.pos> AST
%token <Id.pos> SLASH
%token <Id.pos> MINUS_DOT
%token <Id.pos> PLUS_DOT
%token <Id.pos> AST_DOT
%token <Id.pos> SLASH_DOT
%token <Id.pos> EQUAL
%token <Id.pos> LESS_GREATER
%token <Id.pos> LESS_EQUAL
%token <Id.pos> GREATER_EQUAL
%token <Id.pos> LESS
%token <Id.pos> GREATER
%token <Id.pos> IF
%token THEN
%token ELSE
%token <Id.pos * Id.t> IDENT
%token <Id.pos> LET
%token IN
%token REC
%token COMMA
%token <Id.pos> ARRAY_CREATE
%token <Id.pos> DOT
%token LESS_MINUS
%token <Id.pos> SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* (* ͥ�����̤�associativity���������㤤�������⤤���ء� (caml2html: parser_prior) *) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* (* ���ϵ��������� *) */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* (* ���̤��Ĥ��ʤ��Ƥ��ؿ��ΰ����ˤʤ��뼰 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var(fst $1, snd $1) }
| simple_exp DOT LPAREN exp RPAREN
    { Get($2, $1, $4) }

exp: /* (* ���̤μ� (caml2html: parser_exp) *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not ($1, $2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f) (* -1.23�ʤɤϷ����顼�ǤϤʤ��Τ��̰��� *)
    | e -> Neg($1, e) }
| exp PLUS exp /* (* ­��������ʸ���Ϥ����롼�� (caml2html: parser_add) *) */
    { Add($2, $1, $3) }
| exp MINUS exp
    { Sub($2, $1, $3) }
| exp AST exp
    { Mul($2, $1, $3) }
| exp SLASH exp
    { Div($2, $1, $3) }
| exp EQUAL exp
    { Eq($2, $1, $3) }
| exp LESS_GREATER exp
    { Not($2, Eq($2, $1, $3)) }
| exp LESS exp
    { Not($2, LE($2, $3, $1)) }
| exp GREATER exp
    { Not($2, LE($2, $1, $3)) }
| exp LESS_EQUAL exp
    { LE($2, $1, $3) }
| exp GREATER_EQUAL exp
    { LE($2, $3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($1, $2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($1, $2) }
| exp PLUS_DOT exp
    { FAdd($2, $1, $3) }
| exp MINUS_DOT exp
    { FSub($2, $1, $3) }
| exp AST_DOT exp
    { FMul($2, $1, $3) }
| exp SLASH_DOT exp
    { FDiv($2, $1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let($1, addtyp (snd $2), $4, $6) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($1, $3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| elems
    { Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { LetTuple($1, $3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($2, $1, $4, $7) }
/* 
| exp SEMICOLON exp
    { Let($2, ((Id.gentmp Type.Unit), Type.Unit), $1, $3) }
*/
| semis
    { $1 }

| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($1, $2, $3) }
| error
    { perr_handling (Parsing.symbol_start_pos ()); failwith "parse error" }
    


fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp (snd $1); args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp (snd $1) :: $2 }
| IDENT
    { [addtyp (snd $1)] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp (snd $3)] }
| IDENT COMMA IDENT
    { [addtyp (snd $1); addtyp (snd $3)] }

semis:
| exp SEMICOLON semis
    { Let($2, ((Id.gentmp Type.Unit), Type.Unit), $1, $3) }
| semitail
    { $1 }

semitail:
| exp SEMICOLON exp
    { Let($2, ((Id.gentmp Type.Unit), Type.Unit), $1, $3) }
| exp SEMICOLON
    { Let($2, ((Id.gentmp Type.Unit), Type.Unit), $1, Unit)}

%%

let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
  try
    (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
  with
    | Parsing.Parse_error -> 
      perr_handling lexbuf.Lexing.lex_curr_p; failwith "parse error"
