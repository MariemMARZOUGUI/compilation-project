  {open Location

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
  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)

  (* Initialize the location with the file name *)
  let init_location lexbuf fname =
    Location.init lexbuf fname;
    lexbuf

  (* Function to increase line number in the given buffer *)
  let incr_line lexbuf =
    Location.incr_line lexbuf

  (* Function to get the current position *)
  let curr_position lexbuf =
    Location.curr lexbuf

  (* Function to print a given location *)
  let print_location loc =
    Location.print loc

  (* Define lexer rules *)
  let newline = (['\n' '\r'] | "\r\n")
  let blank = [' ' '\014' '\t' '\012']
  let not_newline_char = [^ '\n' '\r']
  let digit = ['0'-'9']

  rule token = parse
    (* newlines *)
    | newline { incr_line lexbuf; token lexbuf }
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
    | _ as c { 
        let loc = Location.curr lexbuf in
        let pos = curr_position lexbuf in
        let error_msg = Printf.sprintf "Illegal character '%c': " c in
        raise (Location.Error (error_msg, {pos_fname = loc.pos_fname; pos_lnum = pos.pos_lnum; pos_bol = pos.pos_bol; pos_cnum = pos.pos_cnum}))
      }
  }
