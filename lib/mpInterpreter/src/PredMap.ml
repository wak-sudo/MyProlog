(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast

module PredMap : sig
  type predMap
  type pred_def =
    | UserDefined of clause_data list
    | Builtin of (unit -> unit)
  val generate : program -> predMap
  val find_opt : predMap -> term_data -> pred_def option
  val is_empty : predMap -> bool
  val fold : (string * int -> pred_def -> 'acc -> 'acc) -> predMap -> 'acc -> 'acc
end = struct

  type pred_def =
    | UserDefined of clause_data list
    | Builtin of (unit -> unit)

  module PredMapInternal = Map.Make(struct
    type t = string * int
    let compare = compare
  end)

  let is_empty = PredMapInternal.is_empty

  let fold = PredMapInternal.fold

  type predMap = pred_def PredMapInternal.t

  let appendClauseDataToMap (k : string * int) (v : clause_data) (map : predMap) : predMap =
    PredMapInternal.update k
    (function
      | None -> Some (UserDefined [v])
      | Some (UserDefined clauseList) -> Some (UserDefined (clauseList @ [v])) (*/change this/*)
      | Some (Builtin _) -> failwith "cannot add clause to builtin predicate")
    map

  let addClauseData (pred : clause_data) (currentMap : predMap) : predMap =
    match pred with
    | Fact crFact ->
        begin match crFact.data with
        | Sym (factHeadSymbol, argsLst) ->
            appendClauseDataToMap
              (factHeadSymbol.data, List.length argsLst)
              pred
              currentMap
        | _ -> failwith "error"
        end
    | Rule (ruleHead, _) ->
        begin match ruleHead.data with
        | Sym (ruleHeadSymbol, argsLst) ->
            appendClauseDataToMap
              (ruleHeadSymbol.data, List.length argsLst)
              pred
              currentMap
        | _ -> failwith "error"
        end

  let addClause (pred : clause) (currentMap : predMap) : predMap =
    addClauseData pred.data currentMap

  let rec addClauseList (predLst : clause list) (currentMap : predMap) : predMap =
    match predLst with
    | [] -> currentMap
    | x :: xs -> addClauseList xs (addClause x currentMap)

  let generate (prog : program) : predMap = addClauseList prog PredMapInternal.empty

  let find_opt (map : predMap) (t : term_data) : pred_def option =
    match t with
    | Sym(sym, args) -> 
      let key = (sym.data, List.length args) in
      begin
        match PredMapInternal.find_opt key map with
        | None -> None
        | Some v -> Some v
      end
    | _ -> None

end