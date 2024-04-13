{
 (*open Parser*)
 open Utils
  (* used only to execute the main without the parser *)
  type token =
  | EOF | ADD | SUB | MULT | DIV | REM | POP | SWAP| GET | EXEC | LPAR | RPAR | PUSH of int 
  | INT of int

  let print_token = function 
  | EOF   -> print_string "EOF " 
  | ADD   -> print_string "ADD "
  | SUB   -> print_string "SUB "
  | MULT   -> print_string "MULT "
  | DIV   -> print_string "DIV "
  | REM   -> print_string "REM "
  | POP   -> print_string "POP "
  | SWAP  -> print_string "SWAP "
  | PUSH  n -> print_string "PUSH "; print_int n ; print_string " " 
  | INT n -> print_int n ; print_string " " 
  | GET -> print_string   "GET"
  | EXEC -> print_string   "EXEC"
  | LPAR -> print_string  "LPAR"
  | RPAR -> print_string  "RPAR"

  let mk_int nb loc=
    try INT (int_of_string nb)
    with Failure _ -> raise (Location.Error(Printf.sprintf "Illegal integer '%s': " nb,loc))
  
  let mk_push nb lexbuf =
  try PUSH (int_of_string nb lexbuf)
  with Failure _ -> Location.raise_error("Illegal integer: " ^ nb) lexbuf
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
  (* comments *)

  (* commands  *)
  | "PUSH "+ (digit+ as nb) {PUSH (int_of_string nb)}
  | "POP"       { POP }
  | "SWAP"      { SWAP}
  | "ADD"       { ADD }
  | "SUB"       { SUB }
  | "MULT"      { MULT}
  | "DIV"       { DIV }
  | "REM"       { REM }
  | "GET"       { GET }
  | "EXEC"      {EXEC }
  | ")"         {RPAR }
  | "("         {LPAR }

  (* illegal characters *)
  | _ as c { raise (Location.Error(Printf.sprintf "Illegal character '%c': " c, Location.curr lexbuf)) }