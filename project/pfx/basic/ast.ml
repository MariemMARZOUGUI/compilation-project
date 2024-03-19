type command =
| PUSH of int      (* Push an integer onto the stack *)
| ADD              (* Pop two values from the stack, add them, and push the result *)
| SUB              (* Pop two values from the stack, subtract the second from the first, and push the result *)
| MULT             (* Pop two values from the stack, multiply them, and push the result *)
| DIV              (* Pop two values from the stack, divide the first by the second, and push the result *)
| REM              (* Pop two values from the stack, calculate the modulo of the first divided by the second, and push the result *)
| SWAP             (* Swap the top two values on the stack *)
| POP              (* Pop the top value from the stack *)

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

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

