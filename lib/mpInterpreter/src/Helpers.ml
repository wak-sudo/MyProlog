(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast
open Stdlib

let equal_hash h1 h2 eq_val =
  (Hashtbl.length h1 = Hashtbl.length h2) &&
  Hashtbl.fold (fun k v acc ->
    acc && match Hashtbl.find_opt h2 k with
           | Some v2 -> eq_val v v2
           | None -> false
  ) h1 true
  
let equal_symbol (s1 : symbol) (s2 : symbol) = (s1.data = s2.data)
 
let rec equal_term_data (t1 : term_data) (t2 : term_data) : bool =
  match t1, t2 with
  | Var v1, Var v2 -> v1 = v2
  | Num n1, Num n2 -> n1 = n2
  | Atom sym1, Atom sym2 -> equal_symbol sym1 sym2
  | Sym(sym1, lst1), Sym(sym2, lst2) -> equal_symbol sym1 sym2 && List.equal (fun t1 t2 -> equal_term t1 t2) lst1 lst2
  | _ -> false

and equal_term (t1 : term) (t2 : term) = (equal_term_data t1.data t2.data)

let remove_duplicates eq list =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
        if List.exists (fun x -> eq x h) acc then
          aux acc t
        else
          aux (h :: acc) t
  in
  aux [] list

(*/ Change the bool to turn on/off the debug printing. /*)
let debug (s : string) = 
  if false then print_endline(s) else ()