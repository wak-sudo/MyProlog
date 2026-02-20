(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open OUnit2
open TestGreet
open SolverTests

let suite =
  "MpInterpreter tests" >::: [
    "Greet Alice (test)" >:: test_greet;
    "SolverTests.basicOne" >:: SolverTests.basicOne;
    "SolverTests.basicTwo" >:: SolverTests.basicTwo;
    "SolverTests.basicThree" >:: SolverTests.basicThree;
    "SolverTests.ruleOne" >:: SolverTests.ruleOne;
    "SolverTests.perm" >:: SolverTests.perm;
  ]

let () = run_test_tt_main suite