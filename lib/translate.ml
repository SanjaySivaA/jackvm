open Ast

let translate_push (seg, i) =
  let str_i = string_of_int i in
  let prefix = 
  match seg with
  | Constant -> "\n@" ^ str_i ^ "\nD=A\n"
  | Local    -> "\n@LCL\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | Argument -> "\n@ARG\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | This     -> "\n@THIS\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | That     -> "\n@THAT\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | Temp     -> "\n@5\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | Static   -> "\n@16\n" ^ "D=M\n" ^ "@" ^ str_i ^ "\nA=D+A\nD=M\n"
  | Pointer  -> "bla" (*needs work*)
  (*| _        -> failwith "something wrong with translating push\n"*)
  in
  let suffix ="@SP\nA=M\nM=D\n@SP\nM=M+1\n" in
  prefix ^ suffix


  
(*
let translate_statement = function
    | Label(label) -> translate_label label
    | Push(segment, i) -> translate_push (segment, i)
    | Pop(segment, i) -> translate_pop (segment, i)
    | Command command -> translate_command command
*)