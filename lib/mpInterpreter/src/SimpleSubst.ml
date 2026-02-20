(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open Helpers

(*/ It may be a good idea to change it to a map. /*)

module SimpleSubst : sig
  type simpleSubst = (var, term_data) Hashtbl.t
  val equal : simpleSubst -> simpleSubst -> bool
  val empty : simpleSubst
  val add : simpleSubst -> var -> term_data -> unit
  val find_opt : simpleSubst -> var -> term_data option
  val fold : (var -> term_data -> 'acc -> 'acc) -> simpleSubst -> 'acc -> 'acc
  val copy : simpleSubst -> simpleSubst
end = struct
  type simpleSubst = (var, term_data) Hashtbl.t
  let equal (s1 : simpleSubst) (s2 : simpleSubst) : bool = equal_hash s1 s2 equal_term_data
  let empty = Hashtbl.create 32
  let add = Hashtbl.add
  let find_opt = Hashtbl.find_opt
  let fold f (s : simpleSubst) acc = Hashtbl.fold f s acc
  let copy (s : simpleSubst) = Hashtbl.copy s
end