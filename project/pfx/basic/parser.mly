
%{
  (* Ocaml code here*)

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF
%token ADD SUB MULT DIV REM POP SWAP
%token <int> INT
%token <int> PUSH 


(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)
program: i=INT  q=instruction_seq  EOF { i, q }

instruction_seq : 
  | { [] }
  | instr=instruction instrs=instruction_seq { instr :: instrs } 

instruction : 
  | ADD        { ADD }
  | SUB        { SUB }
  | MULT        { MULT }
  | DIV        { DIV }
  | REM        { REM }
  | POP        { POP }
  | SWAP       { SWAP }
  | PUSH n=INT { PUSH n }
%%
