open OUnit
open MyExt
open ListExt
open Automaton

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
  assert_equal (DFA.run maton "01011110") false;
  ()

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
  assert_equal (NFA.run maton "11101010010101011") false;
  ()

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
  assert_equal (DFA.run converted "11101010010101011") false;
  ()

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
  assert_equal (NFA.run maton "00000011111112222220000000") false;
  ()

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
  assert_equal (DFA.run maton "00000011111112222220000000") false;
  ()

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
  assert_equal (DFA.run maton "00001000000") true;
  ()

let helper_nfas _ =
  assert_equal (RegexNFA.run RegexNFA.any "") false;
  assert_equal (RegexNFA.run RegexNFA.any "a") true;
  assert_equal (RegexNFA.run RegexNFA.any "b") true;
  assert_equal (RegexNFA.run RegexNFA.any "abcd") false;

  let just_r = RegexNFA.just "r" in
  assert_equal (RegexNFA.run just_r "") false;
  assert_equal (RegexNFA.run just_r "r") true;
  assert_equal (RegexNFA.run just_r "rw+") false;

  let repeat_r = RegexNFA.repeat just_r in
  assert_equal (RegexNFA.run repeat_r "") true;
  assert_equal (RegexNFA.run repeat_r "r") true;
  assert_equal (RegexNFA.run repeat_r "rrrrr") true;
  assert_equal (RegexNFA.run repeat_r "rrrrrrsrsrr") false;
  ()

let regex _ =
  let any = Regex.compile "." in
  assert_equal (Regex.run any "") false;
  assert_equal (Regex.run any "a") true;
  assert_equal (Regex.run any "b") true;
  assert_equal (Regex.run any "abcd") false;

  let all = Regex.compile ".*" in
  assert_equal (Regex.run all "") true;
  assert_equal (Regex.run all "a") true;
  assert_equal (Regex.run all "b") true;
  assert_equal (Regex.run all "abcd") true;

  let all = Regex.compile "(ab)*" in
  assert_equal (Regex.run all "") true;
  assert_equal (Regex.run all "ab") true;
  assert_equal (Regex.run all "abababab") true;
  assert_equal (Regex.run all "ababababc") false;

  let all = Regex.compile ".(abc)*." in
  assert_equal (Regex.run all "") false;
  assert_equal (Regex.run all "XY") true;
  assert_equal (Regex.run all "XabcY") true;
  assert_equal (Regex.run all "XabcabcabcY") true;
  assert_equal (Regex.run all "XabcabcabcabceYabc") false;
  ()

let suite =
  "suite" >::: [
    "DFA" >:: dfa;
    "NFA" >:: nfa;
    "NFA Converted" >:: nfa_converted;
    "eNFA" >:: enfa;
    "eNFA Converted" >:: enfa_converted;
    "helper NFAs" >:: helper_nfas;
    "regex" >:: regex
  ]

let _ = run_test_tt_main suite
