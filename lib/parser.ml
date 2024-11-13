open Ast

let format_line line =
  let line = String.trim line in
  match String.index_opt line '/' with      
  | Some index -> String.sub line 0 index
  | None -> line

let parse_push (seg, i) = 
  let index = int_of_string i in
  let s = 
  match seg with
  | "local"    -> Local
  | "argument" -> Argument
  | "static"   -> Static
  | "constant" -> Constant
  | "this"     -> This
  | "that"     -> That
  | "pointer"  -> Pointer
  | "temp"     -> Temp
  | _          -> failwith ("Unknown segment: " ^ seg)
  in
  Push (s, index)

let parse_pop (seg, i) =
  let index = int_of_string i in
  let s =
    match seg with
    | "local"    -> Local
    | "argument" -> Argument
    | "static"   -> Static
    | "constant" -> Constant
    | "this"     -> This
    | "that"     -> That
    | "pointer"  -> Pointer
    | "temp"     -> Temp
    | _          -> failwith ("Unknown segment: " ^ seg)
  in
  Pop (s, index)

let parse_line line =
  let line = format_line line in
  let parts = String.split_on_char ' ' line in
  match parts with
  | ["push"; seg; i]   -> Some (parse_push (seg, i))
  | ["pop"; seg; i]    -> Some (parse_pop (seg, i))
  | ["label"; label]   -> Some (Label label)
  | ["goto"; label]    -> Some (Branch (Goto label))
  | ["if-goto"; label] -> Some (Branch (Ifgoto label))
  | ["add"]            -> Some (Command Add)
  | ["sub"]            -> Some (Command Sub)
  | ["neg"]            -> Some (Command Neg)
  | ["eq"]             -> Some (Command Eq)
  | ["gt"]             -> Some (Command Gt)
  | ["lt"]             -> Some (Command Lt)
  | ["and"]            -> Some (Command And)
  | ["or"]             -> Some (Command Or)
  | _                  -> None (*failwith "invalid statement!"*)

let parse_lines lines =
  List.filter_map parse_line lines