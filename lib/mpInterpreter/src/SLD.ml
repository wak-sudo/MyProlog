(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open Event
open MutSubst
open Unify
open PredMap
open SimpleSubst

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
  let continueChn = new_channel () in
  let completionChn = new_channel () in
  
  let getPredList (t : term) : clause_data list =
    match PredMap.find_opt predMap t.data with
      | None -> []
      | Some predDef ->
        match predDef with
        | UserDefined predList -> predList
        | _ -> failwith "to implement - builtin functions"
  in
  
  let solutionsGen (t : term) : (clause_data * simpleSubst) Seq.t = 
    Unify.tryUnify sub t.data (getPredList t)
  in
  
  let solver (q : query) : unit =
    let rec solve (q : query) : unit =
      match q with
      | [] ->
        sync (send completionChn ResultPending);
        sync (receive continueChn);
        MutSubst.reverse sub;

      | leftMostTerm :: restOfQuery -> 
        let solutionsSeq = solutionsGen leftMostTerm in

        let iter p = 
          let fClause = (fst p) in
          let tempSub = (snd p) in
          MutSubst.addSub sub tempSub;
          match fClause with
          | Fact _ -> solve restOfQuery 
          | Rule(_, termList) -> solve (termList @ restOfQuery)
        in

        Seq.iter iter solutionsSeq;
        MutSubst.reverse sub;
    in 
    solve q; 
    sync (send completionChn NoMoreSolutions); 
  in

  let start_generator (q : query) : unit Seq.t =
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