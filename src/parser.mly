%{
    open Expr
%}

%token <int> INT
%token <float> DOUBLE
%token <string> IDENTIFIER
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN LBRACE RBRACE
%token EQUAL EQ NE LT LE GT GE AND OR CARET
%token COMMA SEMICOLON RETURN FN WRITE
%token EOF

%type <string> ident
%type <Expr.util> numeric expr 
%type <Expr.statement list> func_decl_args
%type <Expr.util list> call_args
%type <Expr.block> program block_items block
%type <Expr.statement> block_item var_decl func_decl
%type <string> comparison
%start program

%%
program: block_items EOF                   { $1 }
    ;
block_items: block_item { [$1] }
    | block_items block_item { $1 @ [$2] }
    ;

block_item: var_decl { $1 }
    | func_decl { $1 }
    | RETURN expr SEMICOLON { ReturnStatement($2) }
    | expr SEMICOLON { Expr($1) }
    ;

block: LBRACE block_items RBRACE { $2 }
    | LBRACE RBRACE { [] }
    ;

var_decl: "int" ident ident EQUAL expr SEMICOLON { VariableDeclaration($2, $3, $5) }
    | "int" ident ident SEMICOLON { VariableDeclaration($2, $3, Int(0)) }
    ;

func_decl: FN ident LPAREN func_decl_args RPAREN block
    { 
        let params = List.map (fun stmt -> 
            match stmt with
            | VariableDeclaration(t, id, _) -> (t, id)
            | _ -> failwith "Invalid function parameter"
        ) $4 in
        FunctionDeclaration($2, params, $6)
    }
    ;

func_decl_args: { [] }
    | var_decl { [$1] }
    | func_decl_args COMMA var_decl { $1 @ [$3] }
    ;

ident: IDENTIFIER { $1 }
    ;

numeric: INT { Int($1) }
    | DOUBLE { Double($1) }
    ;

expr: ident EQUAL expr { Assignment($1, $3) }
    | ident LPAREN call_args RPAREN { MethodCall($1, $3) }
    | WRITE LPAREN expr RPAREN { Write($3) }
    | ident { Identifier($1) }
    | numeric { $1 }
    | LPAREN expr RPAREN { $2 }
    | expr comparison expr { BinaryOperator($1, $2, $3) }
    ;

call_args: { [] }
    | expr { [$1] }
    | call_args COMMA expr { $1 @ [$3] }
    ;

comparison: 
    EQ { "==" } 
    | NE { "!=" } 
    | LT { "<" } 
    | LE { "<=" } 
    | GT { ">" } 
    | GE { ">=" } 
    | AND { "&&" } 
    | OR { "||" } 
    ;

%%

