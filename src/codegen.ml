let rec codegen_expr (e: expr) =
    match e with
    | Int n -> const_int int_type n
    | Double n -> const_float double_type n
    | MethodCall (callname, args) -> 
            (*Printf.eprintf "Calling function %s\n" callname;*)
            let callee =  match lookup_function callname the_module with
                            | Some callee -> callee
                            | None -> raise (Error "unknown function referenced")
            in
            let params = params callee in
            if Array.length params == List.length args then () else
                raise (Error "incorrect # args");
            let args = List.map codegen_expr args in
            if callname = "printf" then
                let printval = List.hd args in
                match classify_type (type_of printval) with
                | TypeKind.Integer ->
                    let const_int n = const_int int_type n in
                    let str s = define_global "buf" (const_stringz context s) the_module in
                    let int_spec = build_gep (str "%d\n") [| const_int 0; const_int 0 |] "int_spec" builder in
                build_call callee [| int_spec;  (List.hd args) |] "" builder
                | _ ->
                    let const_int n = const_int int_type n in
                    let str s = define_global "buf" (const_stringz context s) the_module in
                    let int_spec = build_gep (str "%f\n") [| const_int 0; const_int 0 |] "float_spec" builder in
                build_call callee [| int_spec;  (List.hd args) |] "" builder

            else
            build_call callee (Array.of_list args) "calltmp" builder
    | BinaryOperator (lhs, op, rhs) ->
            (*Printf.eprintf "Operator %s\n" op;*)
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            (*Printf.eprintf " type LHS %s" (string_of_lltype (type_of lhs_val));
            Printf.eprintf " type RHS %s\n" (string_of_lltype (type_of rhs_val));*)
            let w =
                match classify_type (type_of lhs_val), classify_type (type_of rhs_val) with
                | TypeKind.Integer, TypeKind.Integer -> 0
                | _, TypeKind.Double -> 1
                | TypeKind.Double, _ -> 1
                | _,_ -> 1
            in
            (
                match op with
                    | "+" -> if w = 0 then build_add lhs_val rhs_val "addtmp" builder else build_fadd lhs_val rhs_val "addtmp" builder
                    | "-" -> if w = 0 then build_sub lhs_val rhs_val "subtmp" builder else build_fsub lhs_val rhs_val "subtmp" builder
                    | "*" -> if w = 0 then build_mul lhs_val rhs_val "multmp" builder else build_fmul lhs_val rhs_val "multmp" builder
                    | "/" -> if w = 0 then build_sdiv lhs_val rhs_val "divtmp" builder else build_fdiv lhs_val rhs_val "multmp" builder
                    | "&&" -> build_and lhs_val rhs_val "andtmp" builder
                    | "||" -> build_or lhs_val rhs_val "ortmp" builder
                    | _ -> failwith "Unsupported"
            )
    | Identifier name ->
            (
            (*Printf.eprintf "Variable %s\n" name;*)
            let v = try Hashtbl.find named_values name with
                | Not_found -> raise (Error "unknown variable name")
            in
            match v with
            | t, v -> (*build_load v name builder*) v
)
