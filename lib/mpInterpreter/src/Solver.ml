(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open MutSubst
open PredMap
open SLD
open Helpers

module Solver : sig
  val solve : program -> query -> (var * term_data) list Seq.t 
end = struct

let rec getVariables (q : query) : var list =
  List.fold_left (fun acc x -> 
    match x.data with
    | Var v -> v :: acc
    | Sym(_, lst) -> getVariables(lst) @ acc
    | _ -> acc)
    [] q

let getDistinctVariables (q : query) : var list =
  remove_duplicates (fun v1 v2 -> v1 = v2) (getVariables q)

let solve (p : program) (q : query) : (var * term_data) list Seq.t =
  let crPredMap = PredMap.generate p in
  let vars = getDistinctVariables q in
  let crSub = MutSubst.empty in
  let solGen = SLD.sld q crPredMap crSub in
  Seq.map (fun _ -> MutSubst.find_simplify_vars crSub vars) solGen

end