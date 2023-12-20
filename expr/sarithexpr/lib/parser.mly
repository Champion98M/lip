%{ open Ast %}

%token TRUE
%token FALSE
%token ZERO
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token ISZERO
%token SUCC
%token PRED
%token EOF

%nonassoc ELSE
%left OR
%left AND
%left NOT
%left PRED SUCC ISZERO


%start <expr> prog

%%

prog:
    | e = expr; EOF { e }
;

expr:
    | TRUE; { True } 
    | FALSE ; { False }
    | ZERO { Zero }
    | NOT; e=expr { Not(e) }
    | e1=expr; AND; e2=expr { And(e1,e2) }
    | e1=expr; OR; e2=expr { Or(e1,e2) }
    | IF; e1 = expr; THEN; e2=expr; ELSE; e3=expr { If(e1,e2,e3) }   
    | SUCC; expr { Succ($2) }
    | PRED; expr { Pred($2) }
    | ISZERO; expr { IsZero($2) }
    | LPAREN; e=expr; RPAREN { e }
;