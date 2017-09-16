open MyExt
open StringExt
open HashtblExt
open ListExt

exception InvalidCharacter

type char_like = Empty | AnyOther | Char of string
type 'a t = {
  alphabet : string list;
  transition : ('a * char_like * 'a list) list;
  initial : 'a list;
  finals : 'a list;
}

let rec triples_find s c = function
  | [] -> raise Not_found
  | (x, y, z)::_ when x = s && y = c -> z
  | _::tl -> triples_find s c tl

let lookup_nexts maton states char =
  let next char state =
    try triples_find state char maton.transition with Not_found ->
    try triples_find state AnyOther maton.transition with Not_found -> [] in
  unions (map (next char) states)

let collect_states trans =
  fold_left (fun acc (s, c, slist) -> union (set_add slist s) acc) [] trans

let cons alph trans inits finals =
  {
    alphabet = alph;
    transition = trans;
    initial = inits;
    finals = finals;
  }

let saturate maton states =
  let lookup_nexts maton states char =
    let next char state =
      try  triples_find state char maton.transition
      with Not_found -> [] in (* don't look for AnyOther *)
    unions (map (next char) states) in
  let rec loop prev =
    let to_add = lookup_nexts maton prev Empty in
    if subset to_add prev then prev
    else loop (union to_add prev) in
  loop states

let transit maton states char =
  saturate maton (lookup_nexts maton (saturate maton states) (Char char))

let run maton str =
  let now = ref (saturate maton maton.initial) in
  for i = 0 to String.length str - 1 do
    now := transit maton !now (String.sub str i 1)
  done;
  inter !now maton.finals <> []

let to_dfa = fun maton ->
  let init = (saturate maton maton.initial) in
  let finals' =
    if anything_in_common init maton.finals
    then ref [init]
    else ref [] in
  let new_trans = ref [] in
  let rec loop searched to_search =
    match to_search with
    | [] -> ()
    | state::tl ->
        let nexts_triplet =
          image (fun c -> state, c, transit maton state c) maton.alphabet in
        let nexts = map (fun (_, _, x) -> x) nexts_triplet in
        new_trans := nexts_triplet @ !new_trans;
        finals' := nexts
                   |> filter (anything_in_common maton.finals)
                   |> union !finals';
        loop (set_add searched state) (union tl (diff nexts (state::searched)))
  in
  let _ = loop [] [init] in
  DFA.cons maton.alphabet !new_trans init !finals'

let string_of_char_like = function
  | Empty -> "ε"
  | AnyOther -> "other"
  | Char c -> c


open Printf
let print_nfa maton string_of_state =
  let print_a_to_b a c b =
    printf "\"%s\" -> \"%s\" [label = \"%s\"]\n"
      (string_of_state a) (string_of_state b) (string_of_char_like c) in
  printf "digraph finite_state_machine {\n";
  printf "rankdir=LR\n";
  printf "node [shape = point] init\n";
  printf "node [shape = ellipse, peripheries=2]\n";
  iter (fun s -> printf "\"%s\"" (string_of_state s)) maton.finals;
  printf "\nnode [shape = ellipse, peripheries=1];\n";
  iter (fun s -> printf "init -> \"%s\"" (string_of_state s)) maton.initial;
  printf "\nnode [shape = ellipse, peripheries=1]\n";
  iter (fun (s, c, slist) -> iter (print_a_to_b s c) slist) maton.transition;
  printf "}\n"

let save_img maton string_of_state filename =
  UnixExt.tee (fun _ -> print_nfa maton string_of_state) "temp"
