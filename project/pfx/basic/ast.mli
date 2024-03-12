(* The type of the commands for the stack machine *)
type command =
| Push of int      (* Push an integer onto the stack *)
| Add              (* Pop two values from the stack, add them, and push the result *)
| Sub              (* Pop two values from the stack, subtract the second from the first, and push the result *)
| Mul              (* Pop two values from the stack, multiply them, and push the result *)
| Div              (* Pop two values from the stack, divide the first by the second, and push the result *)
| Rem              (* Pop two values from the stack, calculate the modulo of the first divided by the second, and push the result *)
| Swap             (* Swap the top two values on the stack *)
| Pop              (* Pop the top value from the stack *)

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
