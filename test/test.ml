open OUnit
open MyExt
open ListExt
open Automaton
open NFA

let save_stdout f filename =
  let oldstdout = Unix.dup Unix.stdout in
  let newstdout = open_out filename in
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;
  f ();
  flush stdout;
  Unix.close (Unix.descr_of_out_channel newstdout);
  Unix.dup2 oldstdout Unix.stdout

let file_diff a b =
  let rec loop () = if (input_line a) <> (input_line b) then false else loop ();
  in try loop () with _ -> true

let stdout_diff f expected =
  let _ = save_stdout f "temp" in
  let ret_val = file_diff (open_in expected) (open_in "temp") in
  Sys.remove "temp";
  ret_val

let assert_output f expected =
  assert_equal (stdout_diff f expected) true

let dfa () =
  (* x accepted <-> x[-1] == 1 *)
  let maton = DFA.cons ["0"; "1"]
      [(0, "0", 0);
       (0, "1", 1);
       (1, "0", 0);
       (1, "1", 1)]
      0 [1] in

  assert_equal (DFA.run maton "") false;
  assert_equal (DFA.run maton "0101111") true;
  assert_equal (DFA.run maton "01011110") false

let nfa () =
  (* regex ".*1..." *)
  let a, b, c, d = 0, 1, 2, 3 in
  let maton = NFA.cons ["0"; "1"]
      [(a, Char "0", [a]);
       (a, Char "1", [a;b]);
       (b, Char "0", [c]);
       (b, Char "1", [c]);
       (c, Char "0", [d]);
       (c, Char "1", [d])]
      [a] [d] in

  assert_equal (NFA.run maton "") false;
  assert_equal (NFA.run maton "100") true;
  assert_equal (NFA.run maton "011011100100") true;
  assert_equal (NFA.run maton "11101010010101011") false

let nfa_converted () =
  (* regex ".*1..." *)
  let a, b, c, d = 0, 1, 2, 3 in
  let maton = NFA.cons ["0"; "1"]
      [(a, Char "0", [a]);
       (a, Char "1", [a;b]);
       (b, Char "0", [c]);
       (b, Char "1", [c]);
       (c, Char "0", [d]);
       (c, Char "1", [d])]
      [a] [d] in


  let converted = (NFA.to_dfa maton) in
  assert_equal (DFA.run converted "") false;
  assert_equal (DFA.run converted "100") true;
  assert_equal (DFA.run converted "011011100100") true;
  assert_equal (DFA.run converted "11101010010101011") false

let enfa () =
  (* regex "[0*][1*][2*]" *)
  let a, b, c = 0, 1, 2 in
  let maton = NFA.cons ["0"; "1"; "2"]
      [(a, Char "0", [a]);
       (a, Empty,    [b]);
       (b, Char "1", [b]);
       (b, Empty,    [c]);
       (c, Char "2", [c])]
      [a] [c] in

  assert_equal (NFA.run maton "") true;
  assert_equal (NFA.run maton "12") true;
  assert_equal (NFA.run maton "01210") false;
  assert_equal (NFA.run maton "000111111111111112222222222") true;
  assert_equal (NFA.run maton "00000011111112222220000000") false

let enfa_converted () =
  (* regex "[0*][1*][2*]" *)
  let a, b, c = 0, 1, 2 in
  let maton = NFA.cons ["0"; "1"; "2"]
      [(a, Char "0", [a]);
       (a, Empty,    [b]);
       (b, Char "1", [b]);
       (b, Empty,    [c]);
       (c, Char "2", [c])]
      [a] [c] in
  let maton = NFA.to_dfa maton in

  assert_equal (DFA.run maton "") true;
  assert_equal (DFA.run maton "12") true;
  assert_equal (DFA.run maton "01210") false;
  assert_equal (DFA.run maton "000111111111111112222222222") true;
  assert_equal (DFA.run maton "00000011111112222220000000") false

let dfa_minimized () =
  let maton = DFA.cons ["0"; "1"]
      [(0, "0", 1);
       (0, "1", 2);
       (1, "0", 0);
       (1, "1", 3);
       (2, "0", 4);
       (2, "1", 5);
       (3, "0", 4);
       (3, "1", 5);
       (4, "0", 4);
       (4, "1", 5);
       (5, "0", 5);
       (5, "1", 5)]
      0 [2;3;4] in
  let maton = DFA.minimize maton in

  assert_equal (DFA.run maton "") false;
  assert_equal (DFA.run maton "1") true;
  assert_equal (DFA.run maton "0000010000") true;
  assert_equal (DFA.run maton "000000111101010011111") false;
  assert_equal (DFA.run maton "00001000000") true

let helper_nfas _ =
  assert_equal (NFA.run NFA.any "") true;
  assert_equal (NFA.run NFA.any "hello world") true;
  assert_equal (NFA.run NFA.any "こんにちは、世界") true;

  assert_equal (NFA.run (NFA.just "r") "") false;
  assert_equal (NFA.run (NFA.just "r") "r") true;
  assert_equal (NFA.run (NFA.just "r") "rw+") false


let suite =
  "suite" >::: [
    "DFA" >:: dfa;
    "NFA" >:: nfa;
    "NFA Converted" >:: nfa_converted;
    "eNFA" >:: enfa;
    "eNFA Converted" >:: enfa_converted;
    "helper NFAs" >:: helper_nfas;
  ]

let _ = run_test_tt_main suite
