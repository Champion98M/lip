%{
open Ast
%}

%token <string> CONST
%token PLUS
%token LPAREN
%token RPAREN
%token EOF
%token SUB
%token MUL
%token DIV
%token <string> HEX

%left PLUS
%left SUB
%left MUL
%left DIV

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | SUB; e1 = expr { Minus(e1) }
  | n = HEX { Hex(int_of_string n) }
  | LPAREN; e=expr; RPAREN {e}
;
