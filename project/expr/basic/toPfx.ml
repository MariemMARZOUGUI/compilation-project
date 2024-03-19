open Ast

let rec generate( exp : Ast.expression ) : BasicPfx.Ast.command list = 
  match exp with
    | Const a -> [Push a]
    | Binop(op,x,y) -> 
      generate x @ generate y @ 
      ( match op with
      | BinOp.Badd -> [Add]
      | BinOp.Bsub -> [Sub]
      | BinOp.Bmul -> [Mult]
      | BinOp.Bdiv -> [Div]
      | BinOp.Bmod -> [Rem]
      )
    | Uminus a -> generate a @ generate (Const 0) @ [Sub]
    | Var _ -> failwith "Not yet supported"
