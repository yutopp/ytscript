(* The type of tokens. *)
type token = 
  | VAR
  | TIMES
  | SEMI
  | RPAREN
  | RBLOCK
  | PLUS
  | MINUS
  | LPAREN
  | LBLOCK
  | INT of (int)
  | ID of (string)
  | GOTO
  | FOR
  | EOF
  | DIV
  | CR
  | COMMA
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val entry: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.ast)

