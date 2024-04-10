{
 open Utils
  
  type token =
  | EOF | ADD | SUB | MUL | DIV | REM | POP | SWAP | PUSH 
  | INT of int

  let print_token = function 
  | EOF   -> print_string "EOF " 
  | ADD   -> print_string "ADD "
  | SUB   -> print_string "SUB "
  | MUL   -> print_string "MUL "
  | DIV   -> print_string "DIV "
  | REM   -> print_string "REM "
  | POP   -> print_string "POP "
  | SWAP  -> print_string "SWAP "
  | PUSH  -> print_string "PUSH "
  | INT n -> print_int n ; print_string " " 

  let mk_int nb loc=
    try INT (int_of_string nb)
    with Failure _ -> raise (Location.Error(Printf.sprintf "Illegal integer '%s': " nb,loc))

}
let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { Location.incr_line lexbuf; token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb { mk_int nb (Location.curr lexbuf) }
  
  (* commands  *)
  | "PUSH"  { PUSH }
  | "POP"       { POP }
  | "SWAP"      { SWAP }
  | "ADD"       { ADD }
  | "SUB"       { SUB }
  | "MUL"       { MUL }
  | "DIV"       { DIV }
  | "REM"       { REM }

  (* illegal characters *)
  | _ as c { raise (Location.Error(Printf.sprintf "Illegal character '%c': " c, Location.curr lexbuf)) }