open MyExt
open StringExt
open HashtblExt
open ListExt

exception InvalidCharacter

type char_like = E | C of char
type 'state transition = ('state, (char_like, 'state list) HashtblExt.t) HashtblExt.t
type 'state nfa = {
  alphabet : char list ;
  transition : 'state transition ;
  initial_states : 'state list ;
  final_states : 'state list ;
}

let collect_states trans_alist =
  let ans = ref [] in
  iter
    (fun (s, alist) ->
       set_add_ref ans s;
       iter (fun (_, s) -> ans := union !ans s) alist) trans_alist;
  !ans

and lookup_nexts maton states char =
  unions
    (map
       (fun state -> try HashtblExt.find (HashtblExt.find maton.transition state) char with Not_found -> [])
       states)

let cons alph trans inits finals =
  let to_assoc tripls =
    let rec loop acc = function
      | [] -> acc
      | (x, y, z)::tl -> loop ((x, (y, z))::acc) tl in
    loop [] tripls in
  let trans = assoc_collect (to_assoc trans) in
  {
    alphabet = alph;
    transition = from_alist (map (fun (s, alist) -> (s, from_alist alist)) trans);
    initial_states = inits;
    final_states = finals
  }

let saturate maton states =
  let rec loop prev =
    let to_add = lookup_nexts maton prev E in
    if subset to_add prev then prev
    else loop (union to_add prev) in
  loop states

let transit maton states char =
  saturate maton (lookup_nexts maton (saturate maton states) (C char))

let to_char_like_list str =
  let rec to_list_rec n l =
    if n < 0
    then l
    else to_list_rec (n - 1) (C str.[n]::l) in
  to_list_rec (String.length str - 1) []

let run maton str =
  let now = ref (saturate maton maton.initial_states) in
  String.iter (fun c ->  now := transit maton !now c) str;
  inter !now maton.final_states <> []

let to_dfa maton =
  let new_init = (saturate maton maton.initial_states) in
  let new_finals =
    if anything_in_common new_init maton.final_states
    then ref [new_init]
    else ref [] in
  let new_trans = ref [] in
  let rec loop searched to_search =
    match to_search with
    | [] -> ()
    | state::rest ->
        let nexts_triplet = image (fun c -> state, c, transit maton state c) maton.alphabet in
        let nexts = image (fun (_, _, x) -> x) nexts_triplet in
        new_trans := nexts_triplet @ !new_trans;
        new_finals := union (filter (anything_in_common maton.final_states) nexts) !new_finals;
        loop (set_add searched state) (union rest (diff nexts (state::searched))) in
  let _ = loop [] [new_init] in
  DFA.cons maton.alphabet !new_trans new_init !new_finals

let string_of_char_like = function
  | E -> "ε"
  | C c -> of_char c

let print_nfa maton string_of_state =
  let print_a_to_b a b label =
    Printf.printf "\"%s\" -> \"%s\" [label = \"%s\"]\n"
      (string_of_state a) (string_of_state b) label in
  Printf.printf "digraph finite_state_machine {\n";
  Printf.printf "rankdir=LR\n";
  Printf.printf "node [shape = point] init\n";
  Printf.printf "node [shape = ellipse, peripheries=2]\n";
  iter (fun s -> Printf.printf "\"%s\"" (string_of_state s)) maton.final_states;
  Printf.printf "\nnode [shape = ellipse, peripheries=1];\n";
  iter (fun s -> Printf.printf "init -> \"%s\"" (string_of_state s)) maton.initial_states;
  Printf.printf "\nnode [shape = ellipse, peripheries=1]\n";
  HashtblExt.iter
    (fun s alist -> HashtblExt.iter
        (fun c s_list -> iter (fun s' -> print_a_to_b s s' (string_of_char_like c)) s_list) alist)
    maton.transition;
  Printf.printf "}\n"
