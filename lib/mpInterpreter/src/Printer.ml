(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open SimpleSubst
open MutSubst
open PredMap

module Printer : sig
  type simpleSubst = SimpleSubst.simpleSubst
  type mutSubst = MutSubst.mutSubst
  type predMap = PredMap.predMap
  val string_of_term_data : term_data -> string
  val string_of_term : term -> string
  val string_of_clause_data : clause_data -> string
  val string_of_clause : clause -> string
  val string_of_hashtbl : ('a -> string) -> ('b -> string) -> ('a, 'b) Hashtbl.t -> string
  val string_of_stack : ('a -> string) -> 'a Stack.t -> string
  val string_of_list : ('a -> string) -> 'a list -> string
  val string_of_query : query -> string
  val string_of_clause_data_list : clause_data list -> string
  val string_of_simpleSubst : simpleSubst -> string
  val string_of_predMap : predMap -> string
  val string_of_mutSubst : mutSubst -> string
end = struct

type simpleSubst = SimpleSubst.simpleSubst
type mutSubst = MutSubst.mutSubst
type predMap = PredMap.predMap
type pred_def = PredMap.pred_def

let rec string_of_term_data (t : term_data) : string =
  match t with
  | Var x -> x
  | Num n -> string_of_int n
  | Atom a -> a.data
  | Sym (f, args) ->
      let args_str =
        args
        |> List.map string_of_term
        |> String.concat ", "
      in
      f.data ^ "(" ^ args_str ^ ")"

and string_of_term (t : term) : string =
  string_of_term_data(t.data)

let string_of_clause_data (c : clause_data) : string =
  let format_body (lst : string list) : string =
    match lst with
    | [] -> "."
    | _ -> String.concat ", " lst ^ "."
  in
  match c with
  | Fact f -> string_of_term f
  | Rule (head, body) -> (string_of_term head) ^ " :- " ^ (format_body 
    (List.fold_left 
    (fun acc v -> (string_of_term v) :: acc) 
    [] body))

let string_of_clause (c : clause) : string =
  string_of_clause_data c.data

let string_of_hashtbl key_to_string value_to_string tbl =
  let items = Hashtbl.fold
    (fun k v acc -> (key_to_string k ^ " -> " ^ value_to_string v) :: acc)
    tbl []
  in "{ " ^ String.concat "; " items ^ " }"

let string_of_stack elem_to_string st =
  let items = Stack.fold (fun acc x -> elem_to_string x :: acc) [] st in
  "[ " ^ String.concat "; " items ^ " ]"

let string_of_list string_of_elem lst =
  "[" ^ String.concat "; " (List.map string_of_elem lst) ^ "]"

let string_of_query (q : query) = string_of_list string_of_term q

let string_of_clause_data_list (q : clause_data list) = string_of_list string_of_clause_data q

let string_of_simpleSubst (s : simpleSubst) = string_of_hashtbl (fun v -> v) (fun t -> string_of_term_data t) s

let string_of_pred_def (p : pred_def) =
  match p with
  | UserDefined clauses -> 
    let clauses_str = List.map (fun c -> "    " ^ string_of_clause_data c) clauses 
    |> String.concat "\n" 
    in "[\n" ^ clauses_str ^ "\n  ]"
  | Builtin _ -> "Builtin"

let string_of_predMap (m : predMap) : string =
    if PredMap.is_empty m then "{}" else
    let entries = PredMap.fold (fun (name, arity) def acc ->
        let entry = 
          Printf.sprintf "Predicate %s/%d:\n  %s" 
          name arity (string_of_pred_def def) 
        in entry :: acc
    ) m []
    in String.concat "\n\n" (List.rev entries)

let string_of_mutSubst (sub : mutSubst) : string = 
  string_of_simpleSubst (MutSubst.getSubCopy sub) ^ "\n" ^ 
  string_of_stack (fun lst -> string_of_list (fun v -> v) lst) (MutSubst.getLogCopy sub) 

end