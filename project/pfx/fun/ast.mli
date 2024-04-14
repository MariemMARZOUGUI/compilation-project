(* The type of the commands for the stack machine *)
type command =
| PUSH of int      (* Push an integer onto the stack *)
| ADD              (* Pop two values from the stack, add them, and push the result *)
| SUB             (* Pop two values from the stack, subtract the second from the first, and push the result *)
| MULT             (* Pop two values from the stack, multiply them, and push the result *)
| DIV             (* Pop two values from the stack, divide the first by the second, and push the result *)
| REM             (* Pop two values from the stack, calculate the modulo of the first divided by the second, and push the result *)
| SWAP            (* Swap the top two values on the stack *)
| POP              (* Pop the top value from the stack *)
| EXEC
| GET
| DoExec of command list

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
