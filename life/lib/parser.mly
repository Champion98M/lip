%{ open Rule %}

%token EOF
%token <string> VALUES
%token SRV
%token BRN
%token SEP
%token EXT

%start <rule> prog

%%

prog:
    | e = expr; EOF {e}
;

expr:
    | SRV; e1=VALUES; SEP; BRN; e2=VALUES {Rule(get_list e1, get_list e2)}
    | EXT; SRV?; e1=VALUES?; SEP; BRN?; e2=VALUES? {Rule((match e1 with Some v -> get_list v | None -> []), (match e2 with Some v -> get_list v | None -> [])) }
;
