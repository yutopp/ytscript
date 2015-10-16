open Batteries

type ast =
    Program of ast list

  | ExprStatement of ast

  | BinaryExpr of ast * string * ast
  | UnaryExpr of string * ast
  | TermExpr of ast

  | ExactInvoke of ast * ast list
  | LabelStatement of string
  | DefVarStatement of string * ast option
  | GotoStatement of string
  | ForStatement of ast option * ast option * ast option * ast

  | Id of string
  | IntLit of int
  | StringLit of string
