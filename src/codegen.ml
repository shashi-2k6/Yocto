open Expr
open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "my_module"
let builder = builder context
let i32_type = Llvm.i32_type context
let i8_type = Llvm.i8_type context
let double_type = Llvm.double_type context
let named_values: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

let rec codegen_expr (e: util) =
  match e with
  | Int n -> const_int i32_type n
  | Double n -> const_float double_type n
  | MethodCall (callname, args) ->
    let callee = match lookup_function callname the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in
    let params = params callee in
    if Array.length params <> List.length args then
      raise (Error "incorrect number of args");
    let args = List.map codegen_expr args in
    if callname = "write" then
      let printval = List.hd args in
      (match classify_type (type_of printval) with
      | TypeKind.Integer ->
          let str s = define_global "buf" (const_stringz context s) the_module in
          let int_spec = build_gep (pointer_type i8_type) (str "%d\n") [| const_int i32_type 0; const_int i32_type 0 |] "int_spec" builder in
          build_call2 (type_of callee) callee [| int_spec; printval |] "" builder
      | TypeKind.Double ->
          let str s = define_global "buf" (const_stringz context s) the_module in
          let float_spec = build_gep (pointer_type i8_type) (str "%f\n") [| const_int i32_type 0; const_int i32_type 0 |] "float_spec" builder in
          build_call2 (type_of callee) callee [| float_spec; printval |] "" builder
      | _ -> raise (Error "unsupported type for write"))
    else
      build_call2 (type_of callee) callee (Array.of_list args) "calltmp" builder
  | BinaryOperator (lhs, op, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      let w = match classify_type (type_of lhs_val), classify_type (type_of rhs_val) with
        | TypeKind.Integer, TypeKind.Integer -> 0
        | _, TypeKind.Double -> 1
        | TypeKind.Double, _ -> 1
        | _ -> failwith "Unsupported types for binary operation"
      in
      (match op with
      | "+" -> if w = 0 then build_add lhs_val rhs_val "addtmp" builder else build_fadd lhs_val rhs_val "addtmp" builder
      | "-" -> if w = 0 then build_sub lhs_val rhs_val "subtmp" builder else build_fsub lhs_val rhs_val "subtmp" builder
      | "*" -> if w = 0 then build_mul lhs_val rhs_val "multmp" builder else build_fmul lhs_val rhs_val "multmp" builder
      | "/" -> if w = 0 then build_sdiv lhs_val rhs_val "divtmp" builder else build_fdiv lhs_val rhs_val "divtmp" builder
      | "&&" -> build_and lhs_val rhs_val "andtmp" builder
      | "||" -> build_or lhs_val rhs_val "ortmp" builder
      | _ -> raise (Error ("Unsupported operator: " ^ op)))
  | Identifier name ->
      let v = try Hashtbl.find named_values name with
        | Not_found -> raise (Error ("unknown variable name: " ^ name))
      in
      v
  | Assignment (name, value) ->
      let value = codegen_expr value in
      let var = try Hashtbl.find named_values name with
        | Not_found -> raise (Error ("unknown variable name: " ^ name))
      in
      ignore (build_store value var builder); value
  | Write expr ->
      let val_to_print = codegen_expr expr in
      let callee = match lookup_function "printf" the_module with
        | Some callee -> callee
        | None -> raise (Error "printf function not found")
      in
      let str_spec = match classify_type (type_of val_to_print) with
        | TypeKind.Integer -> "%d\n"
        | TypeKind.Double -> "%f\n"
        | _ -> raise (Error "unsupported type for write")
      in
      let format_str = define_global "buf" (const_stringz context str_spec) the_module in
      let format_type = pointer_type i8_type in  
      let spec = build_gep format_type format_str [| const_int i32_type 0; const_int i32_type 0 |] "spec" builder in
      build_call2 (type_of callee) callee [| spec; val_to_print |] "" builder
  | Return expr ->
      let ret_val = codegen_expr expr in
      build_ret ret_val builder

let codegen_main (program_block: statement list) output_filename =
  List.iter (fun stmt -> 
    match stmt with
    | Expr e -> ignore (codegen_expr e)  
    | _ -> ()  
  ) program_block;

  let out_channel = open_out output_filename in
  let llvm_ir = Llvm.string_of_llmodule the_module in 
  output_string out_channel llvm_ir;
  close_out out_channel;

