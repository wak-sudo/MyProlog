(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open Stdlib
open MpParser.Ast
open SimpleSubst

module MutSubst : sig
  type simpleSubst = SimpleSubst.simpleSubst
  type mutSubst
  val reverse : mutSubst -> unit
  val addSub : mutSubst -> simpleSubst -> unit
  val find_opt : mutSubst -> var -> term_data option
  val empty : mutSubst
  val subEqual : mutSubst -> mutSubst -> bool
  val find_simplify : mutSubst -> term_data -> term_data
  val find_simplify_vars : mutSubst -> var list -> (var * term_data) list
  val getLogCopy : mutSubst -> var list Stack.t 
  val getSubCopy : mutSubst -> simpleSubst
end = struct
  type logMap = var list Stack.t

  type simpleSubst = SimpleSubst.simpleSubst
  type mutSubst = simpleSubst * logMap

  let empty : mutSubst = (Hashtbl.create 32, Stack.create ())

  let getLog (sub : mutSubst) : logMap = snd sub
  let getSubMap (sub : mutSubst) : simpleSubst = fst sub

  let getLogCopy (m : mutSubst) = Stack.copy (getLog m)
  let getSubCopy (m : mutSubst) = SimpleSubst.copy (getSubMap m)

  let subEqual (s1 : mutSubst) (s2 : mutSubst) =
    SimpleSubst.equal (getSubMap s1) (getSubMap s2)

  let reverse (sub : mutSubst) : unit =
    let log = (getLog sub) in
    if Stack.is_empty log then () else
    let topChanges = Stack.pop log in
    let subMap = getSubMap sub in
    List.iter (fun v -> Hashtbl.remove subMap v) topChanges

  let addVar (sub : simpleSubst) (v : var) (t : term_data) : unit =
    Hashtbl.replace sub v t

  let addSub (sub : mutSubst) (hashTbl : simpleSubst) : unit =
    let subMap = getSubMap sub in
    let log = getLog sub in
    let keyList = Hashtbl.fold (fun key _ acc -> key :: acc) hashTbl [] in
    Hashtbl.iter (fun k v -> addVar subMap k v) hashTbl;
    Stack.push keyList log

  let find_opt (sub : mutSubst) (v : var) : term_data option =
    Hashtbl.find_opt (getSubMap sub) v

  let rec find_simplify (sub : mutSubst) (t : term_data) : term_data =
    match t with
    | Var x -> 
      let res = find_opt sub x in
      begin
        match res with
        | None -> Var x
        | Some t2 -> find_simplify sub t2
      end
    | Sym(s, lst) -> Sym(s, List.map (fun x -> { pos = x.pos; data=(find_simplify sub x.data)} ) lst) (*/ co z pozycją? /*)
    | rest -> rest

  let find_simplify_vars (sub : mutSubst) (tLst : var list) : (var * term_data) list =
    List.fold_left (fun acc k -> (k, (find_simplify sub (Var k))) :: acc) [] tLst

end