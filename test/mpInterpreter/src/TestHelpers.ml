(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpParser.Ast

let dummy_lex_pos =
  { Lexing.pos_fname = "<dummy>";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0 }

let dummy_pos = { start = dummy_lex_pos; length = 0 }

let node data = { pos = dummy_pos; data }

let atom s = Atom (node s)

let natom s = node (atom s)

let var s = node (Var s)

let num i = node (Num i)

let sym s = node (s)

let equalLists l1 l2 eq =
  (List.compare_lengths l1 l2) = 0 &&
  List.for_all (fun el1 -> List.exists (fun el2 -> eq el1 el2) l2) l1 &&
  List.for_all (fun el1 -> List.exists (fun el2 -> eq el1 el2) l1) l2