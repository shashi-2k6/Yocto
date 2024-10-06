open Expr

exception Error of string

let rec eval_expr env e =
  match e with
  | Int n -> float_of_int n
  | Double n -> n
  | Identifier name -> List.assoc name env
  | Assignment (name, value) ->
      let value = eval_expr env value in
      let new_env = (name, value) :: (List.remove_assoc name env) in
      value
  | BinaryOperator (lhs, op, rhs) ->
      let lhs_val = eval_expr env lhs in
      let rhs_val = eval_expr env rhs in
      (match op with
      | "+" -> lhs_val +. rhs_val
      | "-" -> lhs_val -. rhs_val
      | "*" -> lhs_val *. rhs_val
      | "/" -> lhs_val /. rhs_val
      | _ -> raise (Error ("Unsupported operator: " ^ op)))
  | Write expr ->
      let val_to_print = eval_expr env expr in
      print_float val_to_print; print_newline ();
      val_to_print
  | Return expr ->
      eval_expr env expr
  | _ -> raise (Error "Unsupported expression")

let rec interpreter env stmts =
  match stmts with
  | [] -> ()
  | stmt :: rest ->
      let _ = match stmt with
        | Expr e -> eval_expr env e
        | _ -> 0.0  (* Return 0.0 for unsupported statement types *)
      in
      interpreter env rest

let codegen_main (program_block: statement list) =
  interpreter [] program_block

