(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast

module Freshing : sig
  val freshen_clause : clause -> clause
  val freshen_clause_data : clause_data -> clause_data
end = struct

  module VarMap = Map.Make(String)

  type renaming = var VarMap.t

  let fresh_counter = ref 0

  let fresh_var (x : var) : var =
    let n = !fresh_counter in
    incr fresh_counter;
    x ^ "#" ^ string_of_int n

  let rec freshen_term (env : renaming) (t : term) : term * renaming =
    match t.data with
    | Var x ->
      (match VarMap.find_opt x env with
        | Some x' -> ({ t with data = Var x' }, env)
        | None ->
          let x' = fresh_var x in
          ({ t with data = Var x' },
          VarMap.add x x' env))

    | Num _ | Atom _ -> (t, env)

    | Sym (f, args) ->
      let args', env =
        List.fold_left (*/ make it faster /*)
          (fun (acc, env) arg ->
              let arg', env = freshen_term env arg in
              (acc @ [arg'], env))
          ([], env)
          args
      in
      ({ t with data = Sym (f, args') }, env)

  let freshen_clause_data (c : clause_data) : clause_data = 
    let env = VarMap.empty in
    match c with
    | Fact t -> Fact (fst (freshen_term env t))
    | Rule (head, body) ->
      let head', env = freshen_term env head in
      let body', _ =
        List.fold_left
          (fun (acc, env) t ->
              let t', env = freshen_term env t in
              (acc @ [t'], env))
          ([], env)
          body
          in Rule (head', body')
   
  let freshen_clause (c : clause) : clause =
    { pos = c.pos; data = freshen_clause_data c.data }

end