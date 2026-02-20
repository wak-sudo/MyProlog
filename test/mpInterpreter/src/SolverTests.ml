(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open MpInterpreter.Solver
open MpParser.Parser
open MpParser.Ast
open MpInterpreter.Helpers
open TestHelpers 
open OUnit2

module SolverTests : sig
  val perm : 'a -> unit
  val basicOne : 'a -> unit
  val basicTwo : 'a -> unit
  val basicThree : 'a -> unit
  val ruleOne : 'a -> unit
end = struct

let cons (t1 : term_data) (t2 : term_data) = Sym(sym "cons", [node t1; node t2])

let solveCheckGeneral (q : string) (fName : string) (goodRes : (string * term_data) list list) : bool =
  let eq p1 p2 = (fst p1) = (fst p2) && equal_term_data (snd p1) (snd p2) in
  let substAreEqual s1 s2 = (equalLists s1 s2 (fun l1 l2 -> equalLists l1 l2 eq)) in
  let crProgram = parse_file fName in
  let crQuery = parse_query_string q in
  let res = List.of_seq (Solver.solve crProgram crQuery) in
  substAreEqual goodRes res

let getFilePath (s : string) = "testFiles/SolverTests/" ^ s

let perm _ =
  let goodRes = [
  [("P", cons (atom "a") (cons (atom "b") (cons (atom "c") (atom "nil"))))];
  [("P", cons (atom "a") (cons (atom "c") (cons (atom "b") (atom "nil"))))];
  [("P", cons (atom "b") (cons (atom "a") (cons (atom "c") (atom "nil"))))];
  [("P", cons (atom "b") (cons (atom "c") (cons (atom "a") (atom "nil"))))];
  [("P", cons (atom "c") (cons (atom "a") (cons (atom "b") (atom "nil"))))];
  [("P", cons (atom "c") (cons (atom "b") (cons (atom "a") (atom "nil"))))];
  ]
  in
  
  let myQueryStr = "perm(cons(a, cons(b, cons(c, nil))), P)." in
  let testFilePath = getFilePath "perm.txt" in
  let areEqual = solveCheckGeneral myQueryStr testFilePath goodRes in
  assert_bool "Result does not match" areEqual

let basicOne _ = 
  let goodRes = [
  [("X", atom "jablko")];
  [("X", atom "banan")];
  ]
  in
  let myQueryStr = "lubi(ania, X)." in
  let testFilePath = getFilePath "basic1.txt" in
  let areEqual = solveCheckGeneral myQueryStr testFilePath goodRes in
  assert_bool "Result does not match" areEqual

let basicTwo _ = 
  let goodRes = [
  [("X", atom "jablko")];
  [("X", atom "banan")];
  ]
  in
  let myQueryStr = "lubi(ania, X)." in
  let testFilePath = getFilePath "basic2.txt" in
  let areEqual = solveCheckGeneral myQueryStr testFilePath goodRes in
  assert_bool "Result does not match" areEqual

let basicThree _ = 
  let goodRes = [
  [("X", atom "ania")];
  [("X", atom "piotr")];
  ]
  in
  let myQueryStr = "lubi(X, banan)." in
  let testFilePath = getFilePath "basic3.txt" in
  let areEqual = solveCheckGeneral myQueryStr testFilePath goodRes in
  assert_bool "Result does not match" areEqual

let ruleOne _ = 
  let goodRes = [
  [("Wnuk", atom "piotr")];
  [("Wnuk", atom "kasia")];
  ]
  in
  let myQueryStr = "dziadek(anna, Wnuk)." in
  let testFilePath = getFilePath "rule1.txt" in
  let areEqual = solveCheckGeneral myQueryStr testFilePath goodRes in
  assert_bool "Result does not match" areEqual

end



