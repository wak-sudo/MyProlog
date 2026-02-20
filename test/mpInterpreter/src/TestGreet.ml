(*/ Prolog by Wojciech Kieloch 02/2026 /*)

open OUnit2

let greet s = "Hello, " ^ s

let test_greet _ =
  assert_equal "Hello, Alice" (greet "Alice")