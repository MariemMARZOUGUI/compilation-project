  {
  (* open Parser *)
  open Utils

  (* Define a type for tokens *)
  type token = 
    | EOF | ADD | SUB | MULT | DIV | REM | POP | SWAP | PUSH
    | INT of int  

  (* Function to print tokens *)
  let print_token = function 
    | EOF   -> print_string "EOF " 
    | ADD   -> print_string "ADD "
    | SUB   -> print_string "SUB "
    | MULT  -> print_string "MULT "
    | DIV   -> print_string "DIV "
    | REM   -> print_string "REM "
    | POP   -> print_string "POP "
    | SWAP  -> print_string "SWAP "
    | PUSH  -> print_string "PUSH "
    | INT n -> print_int n ; print_string " " 

  (* Function to create an integer token *)
  let mk_int nb loc =
      try INT (int_of_string nb)
      with Failure _ -> raise (Location.Error(Printf.sprintf "Illegal integer '%s': " nb,loc))
  }

  (* Define lexer rules *)
  let newline = (['\n' '\r'] | "\r\n")
  let blank = [' ' '\014' '\t' '\012']
  let not_newline_char = [^ '\n' '\r']
  let digit = ['0'-'9']

  rule token = parse
    | newline { Location.incr_line lexbuf; token lexbuf }
    | blank+ { token lexbuf }
    | eof { EOF }
    | "--" not_newline_char* newline? { token lexbuf }
    | digit+ as nb { mk_int nb (Location.curr lexbuf) }

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
    | _ as c { raise (Location.Error(Printf.sprintf "Illegal character '%c': " c, Location.curr lexbuf)) }
