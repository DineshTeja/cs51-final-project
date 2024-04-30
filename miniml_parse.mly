%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS 
%token TIMES
%token LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE
%token FPLUS FMINUS
%token FTIMES
%token POWER
%token CONCAT
%token LBRACKET RBRACKET APPEND_LST CONS
%token SEMICOLON

%nonassoc IF
%left LESSTHAN EQUALS
%left PLUS MINUS
%left TIMES
%left FPLUS FMINUS
%left FTIMES
%right POWER
%left CONCAT
%nonassoc NEG

%start input
%type <Expr.expr> input

%%
input:  exp EOF                 { $1 }

expr_list:
    exp SEMICOLON expr_list { $1 :: $3 }
  | exp                     { [$1] }

list: 
    LBRACKET expr_list RBRACKET { List $2 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }
        | list                    { $1 }
        | exp CONS exp              { ListCons($1, $3) }
        | exp APPEND_LST exp            { ListAppend($1, $3) } 
          
expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | STRING                { String $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp FPLUS exp         { Binop(Fplus, $1, $3) }
        | exp FMINUS exp        { Binop(Fminus, $1, $3) }
        | exp FTIMES exp        { Binop(Ftimes, $1, $3) }
        | exp POWER exp         { Binop(Power, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | exp CONCAT exp                { Binop(Concat, $1, $3) }
        | LBRACKET expr_list RBRACKET   { Expr.List $2 }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp           { Fun($2, $4) } 
        | RAISE                         { Raise }
        | OPEN exp CLOSE                { $2 }
;
%%