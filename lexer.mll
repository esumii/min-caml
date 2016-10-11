{
(* lexer�����Ѥ����ѿ����ؿ������ʤɤ����� *)
open Parser
open Type

exception SyntaxError of string

let line = ref 1
let end_of_previousline = ref 0
let get_pos lexbuf = (!line, (Lexing.lexeme_start lexbuf)-(!end_of_previousline))
}


let space = [' ' '\t']
let newline = ['\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| newline
    { end_of_previousline := (Lexing.lexeme_end lexbuf);
      line := !line + 1;
      Lexing.new_line lexbuf;
      token lexbuf }
| "(*"
    { comment lexbuf; (* �ͥ��Ȥ����������ȤΤ����Υȥ��å� *)
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT(get_pos lexbuf) }
| digit+ 
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' 
    { MINUS(get_pos lexbuf) }
| '+' 
    { PLUS(get_pos lexbuf) }
| '*'
    { AST(get_pos lexbuf) }
| '/'
    { SLASH(get_pos lexbuf) }
| "-."
    { MINUS_DOT(get_pos lexbuf) }
| "+."
    { PLUS_DOT(get_pos lexbuf) }
| "*."
    { AST_DOT(get_pos lexbuf) }
| "/."
    { SLASH_DOT(get_pos lexbuf) }
| '='
    { EQUAL(get_pos lexbuf) }
| "<>"
    { LESS_GREATER(get_pos lexbuf) }
| "<="
    { LESS_EQUAL(get_pos lexbuf) }
| ">="
    { GREATER_EQUAL(get_pos lexbuf) }
| '<'
    { LESS(get_pos lexbuf) }
| '>'
    { GREATER(get_pos lexbuf) }
| "if"
    { IF(get_pos lexbuf) }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET(get_pos lexbuf) }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(get_pos lexbuf, Id.gentmp Type.Unit) }
| "create_array" (* [XX] ad hoc *)
    { ARRAY_CREATE(get_pos lexbuf) }
| "Array.create"
    { ARRAY_CREATE(get_pos lexbuf) }    
| '.'
    { DOT(get_pos lexbuf) }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON(get_pos lexbuf) }
| eof
    { EOF }
| lower (digit|lower|upper|'_')* (* ¾�Ρ�ͽ�����פ������Ǥʤ��Ȥ����ʤ� *)
    { IDENT(get_pos lexbuf, Lexing.lexeme lexbuf) }
| _
    { Format.eprintf "\x1b[1mline %d, column %d-%d\x1b[0m: @.\x1b[1m\x1b[31mError\x1b[39m\x1b[0m: Syntax error, Unknown token %s @."
    (!line)
    ((Lexing.lexeme_start lexbuf)- (!end_of_previousline))
    ((Lexing.lexeme_end lexbuf)-(!end_of_previousline))
    (Lexing.lexeme lexbuf);
    failwith "lex error" }
    
and comment = parse
| newline
    { end_of_previousline := (Lexing.lexeme_end lexbuf);
      line := !line + 1;
      Lexing.new_line lexbuf;
      comment lexbuf }
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
