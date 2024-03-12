type command =
| Push of int      (* Push an integer onto the stack *)
| Add              (* Pop two values from the stack, add them, and push the result *)
| Sub              (* Pop two values from the stack, subtract the second from the first, and push the result *)
| Mult              (* Pop two values from the stack, multiply them, and push the result *)
| Div              (* Pop two values from the stack, divide the first by the second, and push the result *)
| Swap             (* Swap the top two values on the stack *)
| Pop              (* Pop the top value from the stack *)

type program = int * command list

let string_of_command = function
  | Push n -> "Push " ^ string_of_int n
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mul"
  | Div -> "Div"
  | Swap -> "Swap"
  | Pop -> "Pop"

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

