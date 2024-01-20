open Llvm
open Ctypes



type literal =
  | Int of int
  | Float of float

type binOp =
  | Add
  | Sub
  | Mul
  | Div

type typeExpr =
  | IntType
  | FloatType
  | VoidType

type expr =
  | Literal of literal
  | Binary of binOp * expr * expr
  | FuncDef of string * typeExpr * expr
  | Call of string * expr list

type llvmContext = {
  context : llcontext;
  module_ : llmodule;
  builder : llbuilder;
}

let typeExprToLlvmType (ctx : llvmContext) (typeExpr : typeExpr) : lltype =
  match typeExpr with
  | IntType -> i32_type ctx.context
  | FloatType -> double_type ctx.context
  | VoidType -> void_type ctx.context

(* llvmContext.codegen *)
let codegen (ctx : llvmContext) (expr : expr) : llvalue =
  let rec codegen_expr (expr : expr) : llvalue =
    match expr with
    | Literal (Int i) -> const_int (i32_type ctx.context) i
    | Literal (Float f) -> const_float (double_type ctx.context) f
    | Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin match op with
        | Add -> build_add lhs_val rhs_val "addtmp" ctx.builder
        | Sub -> build_sub lhs_val rhs_val "subtmp" ctx.builder
        | Mul -> build_mul lhs_val rhs_val "multmp" ctx.builder
        | Div -> build_sdiv lhs_val rhs_val "divtmp" ctx.builder
      end
    | FuncDef (name, ret_type, expr) ->
      let func_type = function_type (typeExprToLlvmType ctx ret_type) [||] in
      let func = define_function name func_type ctx.module_ in
      (* let bb = append_block ctx.context "entry" func in *)
      let bb = entry_block func in
      position_at_end bb ctx.builder;
      let stmt = codegen_expr expr in
      let ret_val = codegen_expr expr in
      let _ = build_ret ret_val ctx.builder in 
      func
  in
  codegen_expr expr


let () =
  let context = global_context () in
  let module_ = create_module context "main" in
  let builder = builder context in
  let ctx = { context; module_; builder } in
  let expr = FuncDef ("main", VoidType, Literal (Int 1)) in
  let llval = codegen ctx expr in
  dump_module module_; 

  (* initialize llvm the execution engine *)
  ignore(Llvm_executionengine.initialize ()); 
  (* create the execution engine *)
  let ee = Llvm_executionengine.create ctx.module_ in
  let _ = Llvm_executionengine.initialize () in
  let _ = Llvm_executionengine.add_module module_ ee in
  (* run the function *)
  let funcAddr = Llvm_executionengine.get_function_address "main" void ee in
   ()

   