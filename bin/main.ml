open Lib.Translate 
open Lib.Parser

let translate_vm_to_asm () =
  let lines = ref [] in
  try
    while true do
      lines := input_line stdin :: !lines
    done;
    []
  with End_of_file ->
    List.rev !lines

let write_asm asm_lines =
  List.iter (fun line -> print_endline line) asm_lines

let translate_statements vm_lines =
  let statements = parse_lines vm_lines in
  List.map translate_statement statements

let main () =
  let vm_lines = translate_vm_to_asm () in
  let asm_lines = translate_statements vm_lines in
  write_asm asm_lines

let () = main ()
        