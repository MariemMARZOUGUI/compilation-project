open FunPfx.Ast
open Ast

let generate env dpth =
  let binop_to_Pfx = function
    | BinOp.Badd -> ADD
    | BinOp.Bsub -> SUB
    | BinOp.Bmul -> MULT
    | BinOp.Bdiv -> DIV
    | BinOp.Bmod -> REM in

  let rec gen_expr env dpth = function
    | Const n -> [PUSH n], dpth + 1
    | Binop (op, e1, e2) ->
        let seq2, dpth2 = gen_expr env dpth e2 in
        let seq1, dpth1 = gen_expr env dpth2 e1 in
        seq2 @ seq1 @ [binop_to_Pfx op], dpth1 - 1
    | Uminus e ->
        let seq, dpth2 = gen_expr env dpth e in
        seq @ [PUSH 0; SUB], dpth2
    | App (e1, e2) ->
        let seq2, dpth2 = gen_expr env dpth e2 in
        let seq1, dpth1 = gen_expr env dpth2 e1 in
        seq2 @ seq1 @ [EXEC; SWAP; POP], dpth1 - 1
    | Fun (x, e) ->
        let seq, _ = gen_expr ((x, dpth) :: env) (dpth + 1) e in
        [DoExec seq], dpth
    | Var v ->
        let pos = List.assoc v env in
        [PUSH (dpth - pos); GET], dpth + 1 in

  gen_expr env dpth