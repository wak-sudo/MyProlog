(*/ Prolog by Wojciech Kieloch 02/2026 /*)

(*/ Debug version of SLD.ml /*)

open MpParser.Ast
open Event
open MutSubst
open Unify
open PredMap
open SimpleSubst
open Printer
open Helpers

module SLD : sig 
  type predMap = PredMap.predMap
  type mutSubst = MutSubst.mutSubst
  val sld : query -> predMap -> mutSubst -> unit Seq.t
end = struct

type simpleSubst = SimpleSubst.simpleSubst
type predMap = PredMap.predMap
type mutSubst = MutSubst.mutSubst

type workType =
| NoMoreSolutions
| ResultPending

let sld (q : query) (predMap : predMap) (sub : mutSubst) : unit Seq.t =
  debug("Predmap: " ^ Printer.string_of_predMap predMap);
  let continueChn = new_channel () in
  let completionChn = new_channel () in
  
  let getPredList (t : term) : clause_data list =
    match PredMap.find_opt predMap t.data with
      | None -> []
      | Some predDef ->
        match predDef with
        | UserDefined predList -> debug("Fitting preds: " ^ Printer.string_of_clause_data_list predList); predList
        | _ -> failwith "to implement - builtin functions"
  in
  
  let solutionsGen (t : term) : (clause_data * simpleSubst) Seq.t = 
    Unify.tryUnify sub t.data (getPredList t)
  in
  
  let solver (q : query) : unit =
    let rec solve (q : query) : unit =
      debug("Target: " ^ Printer.string_of_query q);
      match q with
      | [] ->
        debug("Result found, waiting for continuation");
        sync (send completionChn ResultPending);
        debug("Result recieved");
        sync (receive continueChn);
        debug("Continuation signal recieved");
        MutSubst.reverse sub;
        debug("Unification reversed");

      | leftMostTerm :: restOfQuery -> 
        debug("Leftmost term: " ^ Printer.string_of_term leftMostTerm);
        let solutionsSeq = solutionsGen leftMostTerm in

        let iter p = 
          let fClause = (fst p) in
          let tempSub = (snd p) in
          MutSubst.addSub sub tempSub;
          debug("Branch: " ^ Printer.string_of_clause_data fClause);
          debug("\n" ^ Printer.string_of_mutSubst sub ^ "\n");
          match fClause with
          | Fact _ -> solve restOfQuery 
          | Rule(_, termList) -> solve (termList @ restOfQuery)
        in

        Seq.iter iter solutionsSeq;
        MutSubst.reverse sub;
        debug("Unification reversed");
    in 
    solve q; 
    debug("No more solutions"); 
    sync (send completionChn NoMoreSolutions); 
    debug("End signal recieved"); 
  in

  let start_generator (q : query) : unit Seq.t =
    debug("Solution generator started");
    let _ = Thread.create solver q in
    let rec generate () =
        match sync (receive completionChn) with
        | NoMoreSolutions -> Seq.Nil
        | ResultPending -> Seq.Cons((), fun () -> 
            sync (send continueChn ()); 
            generate ())
    in generate

  in start_generator q

end