%token                  FOR VAR GOTO
%token  <int>           INT
%token  <string>        ID
%token                  PLUS MINUS TIMES DIV
%token                  ASSIGN
%token                  LPAREN RPAREN
%token                  LBLOCK RBLOCK
%token                  CR COMMA SEMI EOF

%start <Ast.ast> entry

%%

entry:
        |       statement_list EOF { Ast.Program $1 }

statement_list:
                statement CR* { [$1] }
        |       statement CR+ statement_list { $1::$3 }


(* statements *)

statement:
                defvar_statement { $1 }
        |       goto_statement { $1 }
        |       label_statement { $1 }
        |       for_statement { $1 }
        |       expression_statement { $1 }

for_statement:
                FOR SEMI { Ast.IntLit 1}

label_statement:
                TIMES ID { Ast.LabelStatement $2 }

goto_statement:
                GOTO TIMES ID { Ast.GotoStatement $3 }

defvar_statement:
                VAR ID { Ast.DefVarStatement($2, None) }
        |       VAR ID ASSIGN expression { Ast.DefVarStatement($2, Some $4) }

expression_statement:
                expression { Ast.ExprStatement $1 }


(* expressions *)

expression:
                call_expression { $1 }

call_expression:
                add_sub_expression { $1 }
        |       add_sub_expression argument_list { Ast.ExactInvoke($1, $2) }

add_sub_expression:
                mul_div_rem_expression { $1 }
        |       add_sub_expression PLUS mul_div_rem_expression
                { Ast.BinaryExpr($1, "+", $3) }
        |       add_sub_expression MINUS mul_div_rem_expression
                { Ast.BinaryExpr($1, "-", $3) }

mul_div_rem_expression:
                unary_expression { $1 }
        |       mul_div_rem_expression TIMES unary_expression
                { Ast.BinaryExpr($1, "*", $3) }
        |       mul_div_rem_expression DIV unary_expression
                { Ast.BinaryExpr($1, "/", $3) }


unary_expression:
                postfix_expression { $1 }
        |       MINUS postfix_expression { Ast.UnaryExpr("-", $2) }

postfix_expression:
                primary_expression { $1 }

primary_expression:
                primary_value { $1 }
        |       LPAREN expression RPAREN { $2 }


(* values *)

primary_value:
                identifier { $1 }
        |       int_term { $1 }



identifier:
                ID { Ast.Id $1 }


int_term:
                INT { Ast.IntLit $1 }


(* etc *)

argument_list:
        |       add_sub_expression { [$1] }
        |       add_sub_expression COMMA argument_list { $1::$3 }
