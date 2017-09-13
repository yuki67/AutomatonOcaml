open MyExt
open StringExt
open HashtblExt
open ListExt

exception InvalidCharacter

type 'state transition = ('state, (char, 'state) HashtblExt.t) HashtblExt.t
type 'state dfa = {
  alphabet : char list;
  transition : 'state transition;
  initial_state : 'state;
  final_states : 'state list;
}

let cons alph trans init finals =
  let to_assoc tripls =
    let rec loop acc = function
      | [] -> acc
      | (x, y, z)::tl -> loop ((x, (y, z))::acc) tl in
    loop [] tripls in
  let trans = assoc_collect (to_assoc trans) in
  {
    alphabet =  alph;
    transition = (from_alist (map (fun (s, alist) -> (s, from_alist alist)) trans));
    initial_state = init;
    final_states = finals;
  }

let transit maton state char = HashtblExt.find (HashtblExt.find maton.transition state) char

let run maton str =
  let end_state = fold_left (fun state char -> transit maton state char) maton.initial_state (to_list str) in
  mem end_state maton.final_states

let connected_component udgraph v =
  let rec loop acc = function
    | [] -> acc
    | (x, y) :: tl when x = v || y = v -> loop (union acc [x; y]) tl
    | _ :: tl -> loop acc tl in
  loop [v] udgraph

let minimize (maton : 'state dfa) =
  (* helper functions *)
  let transitable_into marked (p, q) =
    exists
      (fun c ->
         let p', q' = transit maton p c, transit maton q c in
         mem (p', q') marked || mem (q', p') marked)
      maton.alphabet in
  let mark_initial_cond (p, q) =
    (mem p maton.final_states && not (mem q maton.final_states)) ||
    (not (mem p maton.final_states) && mem q maton.final_states) in
  let transitions_from s new_states =
    let origin = find (mem s) new_states
    and next c = find (mem (transit maton s c)) new_states in
    (map (fun c -> origin, c, next c) maton.alphabet) in
  let states = HashtblExt.keys maton.transition in
  let rec loop (marked, unmarked) =
    let to_add = filter (transitable_into marked) unmarked in
    if subset to_add marked then marked, unmarked
    else loop ((union marked to_add), (diff unmarked to_add)) in
  (* main procedure *)
  (* (p, q) ∈ marked <-> p and q are distinguishable *)
  let init_marked, init_unmarked = partition mark_initial_cond (original_pairs states) in
  (* saturate marked and unmarked *)
  (* "marked ∩ unmarked = ∅" stays true after loop *)
  let marked, unmarked = loop (init_marked, init_unmarked) in
  (* calculate new states from marked and unmarked *)
  let new_states =
    fold_left_ignore
      (fun acc s -> exists (fun set -> mem s set) acc)
      (* calculate largest undistinguishable set which contains s *)
      (fun acc s -> (connected_component unmarked s) :: acc)
      [] states in
  (* also calculate new transition from new_states *)
  let new_trans =
    fold_left_ignore
      (fun acc s -> exists (fun (set, _, _) -> mem s set) acc)
      (fun acc s -> (transitions_from s new_states) @ acc)
      [] states in
  cons
    maton.alphabet
    new_trans
    (* note : exactly one element of new_states contains maton.initial_state *)
    (find (mem maton.initial_state) new_states)
    (filter (anything_in_common maton.final_states) new_states)

let print_dfa maton string_of_state =
  let print_a_to_b a b label =
    Printf.printf "\"%s\" -> \"%s\" [label = \"%c\"]\n"
      (string_of_state a) (string_of_state b) label in
  Printf.printf "digraph finite_state_machine {\n";
  Printf.printf "rankdir=LR\n";
  Printf.printf "node [shape = point] init\n";
  Printf.printf "node [shape = ellipse, peripheries=2]\n";
  iter (fun s -> Printf.printf "\"%s\"" (string_of_state s)) maton.final_states;
  Printf.printf "\nnode [shape = ellipse, peripheries=1]\n";
  Printf.printf "init -> \"%s\"" (string_of_state maton.initial_state);
  Printf.printf "\nnode [shape = ellipse, peripheries=1]\n";
  HashtblExt.iter
    (fun s alist -> HashtblExt.iter (fun c s' -> print_a_to_b s s' c)  alist)
    maton.transition;
  Printf.printf "}\n"
