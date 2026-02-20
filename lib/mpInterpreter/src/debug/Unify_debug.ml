(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open MutSubst
open SimpleSubst
open Freshing
open Printer
open Helpers

(*/ Debug version of Unify.ml /*)

module Unify : sig

  type mutSubst = MutSubst.mutSubst
  type simpleSubst = SimpleSubst.simpleSubst

  exception Not_unifiable
  val unify : mutSubst -> term_data -> term_data -> simpleSubst
  val unifyWithClause : mutSubst -> term_data -> clause_data -> simpleSubst
  val tryUnify : mutSubst -> term_data -> clause_data list -> (clause_data * simpleSubst) Seq.t

end = struct

  type mutSubst = MutSubst.mutSubst
  type simpleSubst = SimpleSubst.simpleSubst

  exception Not_unifiable

  let rec contains_var (x : var) (t : term_data) : bool =
    match t with
    | Var y -> x = y
    | Num _ | Atom _ -> false
    | Sym (_, args) -> List.exists (contains_var_t x) args

  and contains_var_t x t = contains_var x t.data

  let unify (generalSub : mutSubst) (t1 : term_data) (t2 : term_data) : simpleSubst =
    debug("Unify: " ^ Printer.string_of_term_data t1 ^ " | " ^ Printer.string_of_term_data t2);
    let crSub = (Hashtbl.create 32) in

    (*/ If the term is a variable, check if there is a substitution for it. /*)
    let rec view (t : term_data) : term_data =
      match t with
      | Var v -> 
        begin
          match MutSubst.find_opt generalSub v with
          | None -> 
            begin
              match Hashtbl.find_opt crSub v with
              | None -> t
              | Some x -> view x
            end 
          | Some x -> view x
        end
      | _ -> t
    in

    let rec aux (t1 : term_data) (t2 : term_data) : unit =
      match view t1, view t2 with

      | Var x, Var y when x = y -> ()

      | Var x, t | t, Var x ->
        if contains_var x t then (debug(x ^ " contained in " ^ Printer.string_of_term_data t); raise Not_unifiable)
        else Hashtbl.replace crSub x t

      | Sym(f1, ts1), Sym(f2, ts2) ->
        if f1.data = f2.data && List.length ts1 = List.length ts2 then
          List.iter2 aux_term ts1 ts2
        else if f1.data <> f2.data then
          (debug(f1.data ^ " and " ^ f2.data ^ " differs by name.");
          raise Not_unifiable)
        else
          (debug(f1.data ^ " and " ^ f2.data ^ " are not equal in lenght.");
          raise Not_unifiable)

      | Atom a1, Atom a2 -> if a1.data <> a2.data then (debug("Atoms " ^ a1.data ^ " and " ^ a2.data ^ " differ"); raise Not_unifiable)
      
      | Num n1, Num n2 -> if n1 <> n2 then (debug("Numbers " ^ string_of_int n1 ^ " and " ^ string_of_int n2 ^ " differ"); raise Not_unifiable) 
      
      | _, _ -> (debug("Default branch"); raise Not_unifiable) 

    and aux_term (t1 : term) (t2 : term) = aux t1.data t2.data
    in
    aux t1 t2; crSub

  let unifyWithClause (generalSub : mutSubst) (t : term_data) (cl : clause_data) : simpleSubst=
    let modUnify x = unify generalSub t x in 
    match cl with | Fact head | Rule (head, _) -> modUnify head.data

  let tryUnify (generalSub : mutSubst) (crTerm : term_data) (targetTermList : clause_data list) : (clause_data * simpleSubst) Seq.t =
    targetTermList |> List.to_seq |> Seq.filter_map 
    (fun clause ->
      let fClause = Freshing.freshen_clause_data clause in
      debug("Fresh clause: " ^ Printer.string_of_clause_data fClause);
      try 
        let result = unifyWithClause generalSub crTerm fClause in
        Some (fClause, result)
      with Not_unifiable -> debug("Not unifiable"); None)
  
end