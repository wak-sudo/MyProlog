(*/ Wojciech Kieloch 02/2026 /*)

open MpParser.Parser
open MpInterpreter.Printer
open MpInterpreter.Solver
open MpParser.Errors
open MpParser.Ast

let string_of_sol (lst : (var * term_data) list) : string =
  Printer.string_of_list (fun p -> "(" ^ (fst p) ^ " <- " ^ Printer.string_of_term_data (snd p) ^ ")") lst

let safe_parse (parseFun : string -> 'a) (arg : string) : ('a, string) result =
  try
    let ast = parseFun arg in
    Ok ast
  with
  | Cannot_open_file { fname; message } ->
      Error (Printf.sprintf "Cannot open file '%s': %s" fname message)
      
  | Parse_error (pos, reason) ->
      let reason_desc = match reason with
        | EofInComment        -> "Unexpected end of file in comment"
        | InvalidNumber s     -> Printf.sprintf "Invalid number: %s" s
        | InvalidChar c       -> Printf.sprintf "Invalid character: '%c'" c
        | UnexpectedToken tok -> Printf.sprintf "Unexpected token: %s" tok
      in
      let line = pos.start.pos_lnum in
      let col = pos.start.pos_cnum - pos.start.pos_bol in
      Error (Printf.sprintf "Parse error at line %d, column %d: %s" line col reason_desc)

  | exn -> Error ("An unexpected error occurred: " ^ Printexc.to_string exn)

let read_loop (solGenFun : query -> (var * term_data) list Seq.t) =
  let rec read_loop_aux (crSolutions : (var * term_data) list Seq.t) (lastQuery : string) =
    print_string "?- ";
    flush stdout;
    
    match read_line () with
    | "#exit" -> ()
    | "#last" -> print_endline lastQuery; read_loop_aux crSolutions lastQuery
    | line -> 
      match line with
      | ";" ->
        begin match crSolutions () with
        | Seq.Nil -> print_endline "No more solutions."; read_loop_aux Seq.empty lastQuery
        | Seq.Cons(x, xs) -> print_endline (string_of_sol x); read_loop_aux xs lastQuery
        end
      | queryStr ->
        begin match safe_parse parse_query_string queryStr with
        | Error msg -> print_endline ("Parsing error: " ^ msg); read_loop_aux crSolutions lastQuery
        | Ok queryAst -> 
          let newSolutions = solGenFun queryAst in
          match newSolutions () with
          | Seq.Nil -> 
              print_endline "false"; 
              read_loop_aux Seq.empty queryStr
          | Seq.Cons(x, xs) -> 
              print_endline (if x = [] then "true" else string_of_sol x); 
              read_loop_aux xs queryStr
        end
  in read_loop_aux Seq.empty "None"
      
let helpMessage = 
"Basic implementation of Prolog by Wojciech Kieloch (02/2026).
Provide the source file path as the first argument.
Then you can interactively enter your queries.
Type:
; to receive the first/next solution.
#exit to quit.
#last to see the active query."

let debugEntranceFlag = false

(*/ Simplfied program entrance used for debugging /*)
let debugStart () =
  let myQuery = "lubi(ania, jablko),lubi(marek, banan)." in
  let crProgram = parse_file "exFiles/test1.txt" in
  let crQuery = parse_query_string myQuery in
  let iter lst =
    if List.is_empty lst then print_endline("true") else
    print_endline( 
    Printer.string_of_list (fun p -> "(" ^ (fst p) ^ " <- " ^ Printer.string_of_term_data (snd p) ^ ")") lst)
  in
  Seq.iter iter (Solver.solve crProgram crQuery)

let () =
  if debugEntranceFlag then debugStart ()
  else
  if Array.length Sys.argv != 2 then
  print_endline "No Prolog source file provided."
  else
  let fstArg = Sys.argv.(1) in
  match fstArg with
  | "help" | "-help" | "--help" -> print_endline helpMessage
  | _ ->
    match safe_parse parse_file fstArg with
    | Error msg -> print_endline ("Parsing error: " ^ msg)
    | Ok program -> read_loop (fun q -> Solver.solve program q)


  