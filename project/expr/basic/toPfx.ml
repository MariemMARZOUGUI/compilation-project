open Ast

let rec generate( exp : Ast.expression ) : BasicPfx.Ast.command list = 
  match exp with
    | Const a -> [PUSH a]
    | Binop(op,x,y) -> 
      generate x @ generate y @ 
      ( match op with
      | BinOp.Badd -> [ADD]
      | BinOp.Bsub -> [SUB]
      | BinOp.Bmul -> [MULT]
      | BinOp.Bdiv -> [DIV]
      | BinOp.Bmod -> [REM]
      )
    | Uminus a -> generate a @ generate (Const 0) @ [SUB]
    | Var _ -> failwith "Not yet supported"
