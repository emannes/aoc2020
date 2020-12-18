%token <int> INT
%token PLUS
%token TIMES
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF 

%start <Arithmetic0.Expression.t> expression 
%%

operation:
    | PLUS { Arithmetic0.Operation.Plus }
    | TIMES { Arithmetic0.Operation.Times }

(* a [partial_expr] looks like "+ 1" or "* (2 + 3) + 4", etc. *)
partial_expr:
(* This uses an inefficient amount of stack space, but it doesn't matter here. See "Parsing Sequences" 
 in https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
    | op = operation ; expr = single_expression ; p = partial_expr 
    { (op, expr) :: p }
    | op = operation ; expr = single_expression { [op, expr ]}
 (*   | (* empty *) { [] } *)

single_expression:
  | LEFT_PAREN ; expr = single_expression; RIGHT_PAREN { expr }
  | i = INT { Int i }
  | expr = single_expression ; p = partial_expr { Arithmetic0.Expression.Expr (expr, p) }

expression: 
    | expr = single_expression ; EOF { expr }
