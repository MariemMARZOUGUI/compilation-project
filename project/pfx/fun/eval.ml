open Ast
open Printf

type exec_or_int =
  | Int of int
  | Executable of command list 

let string_of_exec_or_int = function
  | Int(i) -> string_of_int i
  | Executable(cmmds) -> string_of_commands cmmds

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

  | EXEC ::q, Executable cmmds :: stackTail -> Ok(cmmds@q,stackTail)
  
  | GET :: q, Int n :: stackTail ->
      let rec getNth i = function
        |Int t::q1 -> if i = 0 then t else getNth (i-1) q1 
        |Executable _::q1 -> 
          if i = 0 then -1
          else getNth (i-1) q1
        |[] -> -2
      in
      let nth =  getNth n (Int n::stackTail) in
      if nth = -1 then Error("TypeException error: Wrong type for Get", (q, []))
      else if nth = -2 then Error("Runtime error: Not enough elements for Get", (q, []))
      else
        Ok(q, Int nth :: stackTail)
    | _ -> Error("Not implemented yet", state)

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
