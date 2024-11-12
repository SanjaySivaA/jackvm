type segment =  Local | Argument | Static | Constant | This | That | Pointer | Temp             
            
type command = Add | Sub | Neg | Eq | Gt | Lt | And  | Or

type branch  = Ifgoto | Goto

type label = string

type statement = 
                | Label of label
                | Push of segment * int
                | Pop of segment * int
                | Command of command                 
                
