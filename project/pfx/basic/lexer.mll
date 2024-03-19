{
  (* open Parser *)

  type token = 
  | EOF | ADD | SUB | MUL | DIV | REM | LPAR | RPAR 
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

  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  (***** Exercice 6.1 *****)
  | "ADD"  {ADD}
  | "SUB"  {SUB}
  | "MULT"  {MULT}
  | "DIV"  {DIV}
  | "REM"  {REM}
  | "POP"  {POP}
  | "SWAP" {SWAP}
  | "PUSH" {PUSH}
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }