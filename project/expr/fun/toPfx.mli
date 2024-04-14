(* Function that generate a Pfx program from an Expr program *)
val generate : (string * int) list -> int -> Ast.expression -> FunPfx.Ast.command list * int