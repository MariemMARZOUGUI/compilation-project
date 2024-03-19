{
  open Parser

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
  | "Add"  {Add}
  | "Sub"  {Sub}
  | "Mult"  {Mult}
  | "Div"  {Div}
  | "Rem"  {Rem}
  | "Pop"  {Pop}
  | "Swap" {Swap}
  | "Push" {Push}
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }

let rec examine_all lexbuf = 
    let result = token lexbuf in 
    print_token result;
    print_string "";
    match result with
    | EOF -> ()
    | _ -> examine_all lexbuf

  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")
  let _ = Arg.parse [] compile ""