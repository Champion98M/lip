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

%token INT 
%token BOOL
%token LBRACE RBRACE

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
  | FALSE; { False }
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
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e,c1,c2) }
  | WHILE; e = expr; DO; c = cmd { While(e,c) }
  | LPAREN; c=cmd; RPAREN { c }
  | LBRACE; d=decl; c=cmd; RBRACE { Decl(d,c) }
  | c1 = cmd; SM; c2=cmd { Seq(c1,c2) }
;

decl: 
  | { EmptyDecl }
  | INT; VAR; SM; d=decl { IntVar($2,d) }
  | BOOL; VAR; SM d=decl { BoolVar($2,d) }

(*
%token <int> CONST "42"
%token <string> VAR "x"
%token TRUE "true" FALSE "false"
%token ADD "+" SUB "-" MUL "*"
%token NOT "not" AND "and" OR "or" EQ "=" LEQ "<="
%token ASSIGN ":="
%token IF "if" THEN "then" ELSE "else"
%token WHILE "while" DO "do"
%token SM ";"
%token LPAREN "(" RPAREN ")"
%token SKIP "skip"
%token INT "int" BOOL "bool"
%token LBRACE "{" RBRACE "}"
%token EOF

%start <Ast.cmd> main
%{ open Ast %}

%left ";"
%nonassoc "else" "do"

%%

(* Most precedences are encoded in a stratified grammar. *)

let main :=
  ~ = cmd; EOF; <>

let cmd :=
| "skip"; { Skip }
| "while"; ~ = expr; "do"; ~ = cmd; <While>
| "if"; e = expr; "then"; c1 = cmd; "else"; c2 = cmd; { If (e,c1,c2) }
| ~ = identifier; ":="; ~ = expr; <Assign>
| "("; ~ = cmd; ")"; <>
| "{"; ~ = decl; ~ = cmd; "}"; <Decl>
| c1 = cmd; ";"; c2 = cmd; { Seq (c1,c2) }

let decl :=
| { EmptyDecl }
| "int"; ~ = identifier; ";"; ~ = decl; <IntVar>
| "bool"; ~ = identifier; ";"; ~ = decl; <BoolVar>

let expr == logical_or_expr

let logical_or_expr :=
| logical_and_expr
| ~ = logical_or_expr; "or"; ~ = logical_and_expr; <Or>

let logical_and_expr :=
| logical_not_expr
| ~ = logical_and_expr; "and"; ~ = logical_not_expr; <And>

let logical_not_expr :=
| equality_expr
| "not"; ~ = logical_not_expr; <Not>

let equality_expr :=
| additive_expr
| ~ = equality_expr; "="; ~ = additive_expr; <Eq>
| ~ = equality_expr; "<="; ~ = additive_expr; <Leq>

let additive_expr :=
| multiplicative_expr
| ~ = additive_expr; "+"; ~ = multiplicative_expr; <Add>
| ~ = additive_expr; "-"; ~ = multiplicative_expr; <Sub>

let multiplicative_expr :=
| atomic_expr
| ~ = multiplicative_expr; "*"; ~ = atomic_expr; <Mul>

let atomic_expr :=
| constant
| ~ = identifier; <Var>
| "("; ~ = expr; ")"; <>

let constant :=
| "true"; { True }
| "false"; { False }
| ~ = "42"; <Const>

let identifier :=
| ~ = "x"; <>*)