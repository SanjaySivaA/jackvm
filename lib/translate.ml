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
  | Pointer  -> "\n@" ^ (if i = 0 then "THIS" else "THAT") ^ "\nD=M\n"
  in
  let suffix ="@SP\nA=M\nM=D\n@SP\nM=M+1\n" in
  prefix ^ suffix

let translate_pop (seg, i) = 
  let str_i = string_of_int i in 
  let prefix = 
  match seg with
  | Constant -> failwith "pop does not take argument constant"
  | Local    -> "\n@LCL\nD=M\n@" ^ str_i ^ "\nD=D+A\n@R13\nM=D\n" 
  | Argument -> "\n@ARG\nD=M\n@" ^ str_i ^ "\nD=D+A\n@R13\nM=D\n" 
  | This     -> "\n@THIS\nD=M\n@" ^ str_i ^ "\nD=D+A\n@R13\nM=D\n" 
  | That     -> "\n@THAT\nD=M\n@" ^ str_i ^ "\nD=D+A\n@R13\nM=D\n" 
  | Temp     -> "\n@5\nD=A\n@" ^ str_i ^ "\nD=D+A\n@R13\nM=D\n" 
  | Static   -> "\n@Static." ^ str_i ^ "\nD=A\n@R13\nM=D\n" 
  | Pointer  -> "\n@" ^ (if i = 0 then "THIS" else "THAT") ^ "\nD=A\n@R13\nM=D\n" 
  in 
  let suffix = "@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n" in
  prefix ^ suffix

  let translate_command = function 
  | Add -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M+D\n" 
  | Sub -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M-D\n" 
  | Neg -> "@SP\nA=M-1\nM=-M\n" 
  | Eq -> "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@TRUE\nD;JEQ\n@SP\nA=M-1\nM=0\n@CONT\n0;JMP\n(TRUE)\n@SP\nA=M-1\nM=-1\n(CONT)\n" 
  | Gt -> "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@TRUE\nD;JGT\n@SP\nA=M-1\nM=0\n@CONT\n0;JMP\n(TRUE)\n@SP\nA=M-1\nM=-1\n(CONT)\n" 
  | Lt -> "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@TRUE\nD;JLT\n@SP\nA=M-1\nM=0\n@CONT\n0;JMP\n(TRUE)\n@SP\nA=M-1\nM=-1\n(CONT)\n" 
  | And -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M&D\n" 
  | Or -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M|D\n" 
  | Not -> "@SP\nA=M-1\nM=!M\n"  

let translate_label label = 
  "\n(" ^ label ^ ")\n"

let translate_statement = function
    | Label(label) -> translate_label label
    | Push(segment, i) -> translate_push (segment, i)
    | Pop(segment, i) -> translate_pop (segment, i)
    | Command command -> translate_command command