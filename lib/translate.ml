open Ast

let label_counter = ref 0
let generate_label prefix =
  let count = string_of_int !label_counter in
  let label =  prefix ^ count in
  label_counter := !label_counter + 1;
  label

let translate_push (seg, i) =
  let str_i = string_of_int i in
  match seg with
  | Constant -> "@" ^ str_i ^ "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | Local    -> "@LCL\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | Argument -> "@ARG\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | This     -> "@THIS\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | That     -> "@THAT\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | Temp     -> "@5\nD=A\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | Static   -> "@" ^ "file." ^ str_i ^ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"
  | Pointer  -> "@" ^ (if i = 0 then "THIS" else if i=1 then "THAT" else failwith "invalid syntax for pointer") ^ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1"

let translate_pop (seg, i) = 
  let str_i = string_of_int i in 
  match seg with
  | Constant -> failwith "pop does not take argument constant"
  | Local    -> "@LCL\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@13\nA=M\nM=D" 
  | Argument -> "@ARG\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@13\nA=M\nM=D" 
  | This     -> "@THIS\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@13\nA=M\nM=D" 
  | That     -> "@THAT\nD=M\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@13\nA=M\nM=D" 
  | Temp     -> "@5\nD=A\n@13\nM=D\n@" ^ str_i ^ "\nD=A\n@13\nM=D+M\n@SP\nM=M-1\nA=M\nD=M\n@13\nA=M\nM=D" 
  | Static   -> "@file." ^ str_i ^ "\nD=A\n@SP\nAM=M-1\nD=M\n@file." ^ str_i ^ "\nM=D" 
  | Pointer  -> "@SP\nM=M-1\nA=M\nD=M\n@" ^ (if i = 0 then "THIS" else if i=1 then "THAT" else failwith "invalid syntax for pointer") ^ "\nM=D" 

  let translate_command = function 
  | Add -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=D+M"
  | Sub -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M-D" 
  | Neg -> "@SP\nA=M-1\nM=-M" 
  | Eq -> 
          let base_label = generate_label "EQ" in
          let if_label = "if_" ^ base_label in
          let else_label = "else_" ^ base_label in
          "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@" ^ if_label ^ "\nD;JEQ\nD=0\n@" ^ else_label ^ "\n0;JMP\n(" ^ if_label ^ ")\nD=-1\n(" ^ else_label ^ ")\n@SP\nA=M-1\nM=D" 
  | Gt ->
    let base_label = generate_label "GT" in
    let if_label = "if_" ^ base_label in
    let else_label = "else_" ^ base_label in
      "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@" ^ if_label ^ "\nD;JGT\nD=0\n@" ^ else_label ^ "\n0;JMP\n(" ^ if_label ^ ")\nD=-1\n(" ^ else_label ^ ")\n@SP\nA=M-1\nM=D"
  | Lt ->
          let base_label = generate_label "LT" in
          let if_label = "if_" ^ base_label in
          let else_label = "else_" ^ base_label in
            "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@" ^ if_label ^ "\nD;JLT\nD=0\n@" ^ else_label ^ "\n0;JMP\n(" ^ if_label ^ ")\nD=-1\n(" ^ else_label ^ ")\n@SP\nA=M-1\nM=D" 
  | And -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=D&M" 
  | Or -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=D|M" 
  | Not -> "@SP\nA=M-1\nM=!M"  

let translate_label label = 
  "(" ^ label ^ ")\n"

let translate_statement = function
    | Label(label) -> translate_label label
    | Push(segment, i) -> translate_push (segment, i)
    | Pop(segment, i) -> translate_pop (segment, i)
    | Command command -> translate_command command
    | _               -> "working on branch"