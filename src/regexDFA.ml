open MyExt
open StringExt
open HashtblExt
open ListExt

module OrderedInt =
struct
  type t = int
  let compare = Pervasives.compare
end
module IntMap = MapExt.Make (OrderedInt)

module StringMap = MapExt.Make (String)

type state = int
type transition = (state StringMap.t) IntMap.t
type t = {
  alphabet : string list;
  transition : transition;
  init : state;
  finals : state list;
}

let cons alph trans init finals =
  let to_map triples =
    fold_left (fun acc (x, y, z) -> (hash x, (y, hash z))::acc) [] triples
    |> assoc_collect
    |> map (fun (s, alist) -> (s, StringMap.from_alist alist))
    |> IntMap.from_alist in
  {
    alphabet =  alph;
    transition = to_map trans;
    init = hash init;
    finals = map hash finals;
  }

let transit maton state char =
  StringMap.find char (IntMap.find state maton.transition)

let run maton str =
  let now = ref maton.init in
  for i = 0 to String.length str - 1 do
    now := transit maton !now (String.sub str i 1)
  done;
  mem !now maton.finals


let minimize maton =
  (* helper functions *)
  let transitable_into marked (p, q) =
    exists
      (fun c ->
         let p', q' = transit maton p c, transit maton q c in
         mem (p', q') marked || mem (q', p') marked)
      maton.alphabet in
  let mark_initial_cond (p, q) =
    (mem p maton.finals && not (mem q maton.finals)) ||
    (not (mem p maton.finals) && mem q maton.finals) in
  let transitions_from s new_states =
    let origin = find (mem s) new_states
    and next c = find (mem (transit maton s c)) new_states in
    (map (fun c -> origin, c, next c) maton.alphabet) in
  let states = map (fun (x, _) -> x) (IntMap.bindings maton.transition) in
  let rec loop (marked, unmarked) =
    let to_add = filter (transitable_into marked) unmarked in
    if subset to_add marked then marked, unmarked
    else loop ((union marked to_add), (diff unmarked to_add)) in
  let connected_component udgraph v =
    let rec loop acc = function
      | [] -> acc
      | (x, y) :: tl when x = v || y = v -> loop (union acc [x; y]) tl
      | _ :: tl -> loop acc tl in
    loop [v] udgraph in
  (* main procedure *)
  (* (p, q) ∈ marked <-> p and q are distinguishable *)
  let init_marked, init_unmarked =
    partition mark_initial_cond (original_pairs states) in
  (* saturate marked and unmarked *)
  (* "marked ∩ unmarked = ∅" stays true after loop *)
  let marked, unmarked = loop (init_marked, init_unmarked) in
  (* calculate new states from marked and unmarked *)
  let new_states =
    fold_left_ignore
      (fun acc s -> exists (fun set -> mem s set) acc)
      (* largest undistinguishable set which contains s *)
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
    (* note : exactly one element of new_states contains maton.init *)
    (find (mem maton.init) new_states)
    (filter (anything_in_common maton.finals) new_states)

open Printf
let print_dfa maton =
  let string_of_state = string_of_int in
  let print_a_to_b a b label =
    printf "\"%s\" -> \"%s\" [label = \"%s\"]\n"
      (string_of_state a) (string_of_state b) label in
  printf "digraph finite_state_machine {\n";
  printf "rankdir=LR\n";
  printf "node [shape = point] init\n";
  printf "node [shape = ellipse, peripheries=2]\n";
  iter
    (fun s -> printf "\"%s\"" (string_of_state s))
    maton.finals;
  printf "\nnode [shape = ellipse, peripheries=1]\n";
  printf "init -> \"%s\"" (string_of_state maton.init);
  printf "\nnode [shape = ellipse, peripheries=1]\n";
  IntMap.iter
    (fun s smap -> StringMap.iter (fun c s' ->  print_a_to_b s s' c) smap)
    maton.transition;
  printf "}\n"
