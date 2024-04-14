open Ast

exception RuntimeError of string

let rec eval env = function
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise(RuntimeError("Unbound variable "^v)))
  | Binop(op,e1,e2) ->
     begin
       match op,eval env e2 with
       | (Bdiv | Bmod), 0 -> raise(RuntimeError("division by zero"))
       | _, v -> (BinOp.eval op) (eval env e1) v
     end
  | Uminus e -> - (eval env e)
  | App (_, _) | Fun (_, _) -> (* Ne pas évaluer les fonctions et les applications *)
      raise (RuntimeError "Functions and applications cannot be evaluated directly")
