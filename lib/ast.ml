type segment =  Local | Argument | Static | Constant | This | That | Pointer | Temp             
            
type command = Add | Sub | Neg | Eq | Gt | Lt | And  | Or | Not

type label = string

type branch  = 
  | Ifgoto of label
  | Goto of label

type statement = 
                | Label of label
                | Push of segment * int
                | Pop of segment * int
                | Command of command                 
                | Branch of branch
