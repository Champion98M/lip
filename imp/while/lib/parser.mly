%{ open Ast %}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token <string> VAR
%token <string> CONST
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SM
%token EOF

%token SKIP
%token ASSIGN
%token WHILE
%token DO

%left SM
%left ELSE DO
%left OR AND
%left NOT
%left EQ LEQ

%left ADD SUB
%left MUL


%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;;

expr:
  | VAR { Var($1) }
  | CONST { Const(int_of_string $1) }
  | TRUE; { True } 
  | FALSE ; { False }
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | LPAREN; e=expr; RPAREN { e }
;

cmd:
  | SKIP { Skip }
  | VAR; ASSIGN; e = expr; { Assign($1,e) }
  | c1 = cmd; SM; c2=cmd { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e,c1,c2) }
  | WHILE; e = expr; DO; c = cmd { While(e,c) }
  | LPAREN; c=cmd; RPAREN { c }
;