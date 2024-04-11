type command =
  | PUSH of int
  | ADD
  | SUB
  | MULT
  | DIV
  | REM
  | SWAP
  | POP
  | Q of command list
  | EXEC
  | GET

type program = int * command list

let string_of_command = function
  | PUSH n -> "PUSH" ^ string_of_int n
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | REM -> "REM"
  | SWAP -> "SWAP"
  | POP -> "POP"
  | Q cmds -> "Q " ^ string_of_commands cmds
  | EXEC -> "EXEC"
  | GET -> "GET"

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)
