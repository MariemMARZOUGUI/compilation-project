open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  (* | DefineMe :: q , stack          -> Ok (q, stack) *)
  | [], _ -> Error("Nothing to step",state)
  | PUSH n :: q , stack -> Ok (q, n::stack)

  | POP :: q, stack -> 
    ( match stack with 
      | _ :: s -> Ok (q, s)
      | _ -> Error("Empty stack", ([], stack))
    ) 

  | SWAP :: q, stack -> 
    ( match stack with 
      | v1:: v2 :: s -> Ok (q, v2 :: v1 :: s)
      | _ -> Error("Empty stack or no enough elements", ([], stack))
    ) 
  
  | ADD :: q, stack ->
    ( match stack with 
      | v1 :: v2 :: s -> Ok (q, (v1+v2) :: s)
      | _ -> Error("Not enough arguments for addition", ([], stack))
    )

  | SUB ::q , stack ->
    ( match stack with
     | v1 :: v2 ::s -> Ok (q, (v1-v2)::s)
     | _ -> Error("Not enough arguments for substraction", ([], stack))
    )
  
  | DIV ::q , stack ->
    ( match stack with
      | v1 :: v2 ::s -> Ok (q, (v1/v2)::s)
      | _ -> Error("Not enough arguments for division", ([], stack))
    )
  
  | MULT ::q , stack ->
    ( match stack with
      | v1 :: v2 ::s -> Ok (q, (v1*v2)::s)
      | _ -> Error("Not enough arguments for multiplication", ([], stack))
    )

  | REM :: q, stack -> 
    ( match stack with
      | v1 :: v2 :: s -> Ok (q, (v1 mod v2) :: s)
      | _ -> Error("Not enough arguments for mod", ([], stack))
    )

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
