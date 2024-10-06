type util = 
  | Int of int
  | Double of float
  | Assignment of string * util
  | MethodCall of string * util list
  | BinaryOperator of util * string * util
  | Identifier of string
  | Return of util
  | Write of util  

type variableList = string * string * util list 

type statement = 
    | Expr of util
    | VariableDeclaration of string * string * util
    | VariableDeclarationExpr of string * string * util
    | FunctionDeclaration of string * (string * string) list * statement list
    | ReturnStatement of util

type block = statement list


let rec 
  print_expr_r (el : util list) : unit =
      match el with
      | hd::tl -> print_expr hd; print_expr_r tl
      | [] -> ()
and 
  print_expr (e:util) : unit =
      match e with
      | Int(i) -> Printf.printf " %i " i
      | Double(d) -> Printf.printf " %f " d
      | Assignment(s, e) -> Printf.printf " %s = " s; print_expr(e); print_newline();
      | MethodCall (s, el) -> Printf.printf " %s(" s; print_expr_r el; Printf.printf ")"; print_newline ();
      | BinaryOperator (e1, s, e2) -> print_expr e1; Printf.printf  " %s " s; print_expr e2;
      | Identifier(s) -> Printf.printf " %s" s
      | Return(e) -> Printf.printf "return "; print_expr e; print_newline()
      | Write(e) -> Printf.printf "write("; print_expr e; Printf.printf ")"; print_newline()
and 
  print_statement (s: statement): unit =
    match s with 
    | Expr(expr) -> print_expr expr
    | VariableDeclaration(t, id, e) -> Printf.printf "%s %s = " t id; print_expr e; print_newline ();
    | VariableDeclarationExpr(t, id, e) -> Printf.printf "%s %s = " t id; print_expr e;
    | FunctionDeclaration(name, params, body) -> 
        Printf.printf "fn %s(" name;
        List.iter (fun (t, id) -> Printf.printf "%s %s, " t id) params;
        Printf.printf ")\n{\n";
        print_block_r body;
        Printf.printf "}\n";
    | ReturnStatement(e) -> print_expr (Return e)  
and 
  print_block_r (b:block): unit = 
    match b with
    | hd::tl -> print_statement hd; print_block_r tl
    | [] -> ()

let print_block (b:block) = 
  print_block_r b;
  print_newline ();
  b

