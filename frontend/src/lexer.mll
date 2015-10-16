{
  open Parser
  open Lexing

  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}


rule token = parse
    [' ' '\t']          { token lexbuf }
  | '\n'                { next_line lexbuf; CR }

  | "for"               { FOR }
  | "var"               { VAR }
  | "goto"              { GOTO }

  | ['0'-'9']+ as i     { INT (int_of_string i) }
  | ['a'-'z' '_']+ as s { ID s }

  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV }
  | '='                 { ASSIGN }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }

  | ','                 { COMMA }

  | ';'                 { SEMI }

  | '{'                 { LBLOCK }
  | '}'                 { RBLOCK }

  | eof                 { EOF }

  | _ {
        let pos = Lexing.lexeme_start_p lexbuf in
        let line_num = pos.Lexing.pos_lnum in
        let column_pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        let mes = Printf.sprintf "unexpected character(Line: %d, Pos: %d)\n" line_num column_pos in
        raise (Error mes)
      }
