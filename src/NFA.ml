open MyExt
open StringExt
open HashtblExt
open ListExt

exception InvalidCharacter

module OrderedInt =
struct
  type t = int
  let compare = Pervasives.compare
end
module IntMap = MapExt.Make (OrderedInt)

type char_like = Empty | AnyOther | Char of string
module OrderedCharLike =
struct
  type t = char_like
  let compare x y =
    match x, y with
    | Empty, Empty
    | AnyOther, AnyOther -> 0
    | Empty, AnyOther -> 1
    | AnyOther, Empty -> -1
    | Char p, Char q -> String.compare p q
    | _, AnyOther -> 2
    | _, Empty -> 1
    | AnyOther, _ -> -2
    | Empty, _ -> -1
end
module CharLikeMap = MapExt.Make (OrderedCharLike)

type state = int
type transition = (state list CharLikeMap.t) IntMap.t
type 'a t = {
  alphabet : string list;
  transition : transition;
  initial_states : state list;
  final_states : state list;
  dictionary : (state * 'a) list
}

let lookup_nexts maton (states:state list) (char:char_like) =
  let next char state =
    try
      CharLikeMap.find char (IntMap.find state maton.transition)
    with Not_found ->
    try
      CharLikeMap.find AnyOther (IntMap.find state maton.transition)
    with Not_found -> [] in
  unions (map (next char) states)

let cons ?(dictate=true) alph trans inits finals =
  let collect_states trans =
    fold_left (fun acc (s, c, slist) -> union (set_add slist s) acc) [] trans in
  let to_map triples =
    let assoc = fold_left (fun acc (x, y, z) -> (hash x, (y, map hash z))::acc) [] triples in
    let collected = assoc_collect assoc in
    IntMap.from_alist (map (fun (s, alist) -> (s, CharLikeMap.from_alist alist)) collected) in
  let dic = if dictate then map (fun s -> (hash s, s)) (collect_states trans) else [] in
  {
    alphabet = alph;
    transition = to_map trans;
    initial_states = map hash inits;
    final_states = map hash finals;
    dictionary = dic
  }

let saturate maton states =
  let rec loop prev =
    let to_add = lookup_nexts maton prev Empty in
    if subset to_add prev then prev
    else loop (union to_add prev) in
  loop states

let transit maton states char =
  saturate maton (lookup_nexts maton (saturate maton states) (Char char))

let run maton str =
  let now = ref (saturate maton maton.initial_states) in
  for i = 0 to String.length str - 1 do
    now := transit maton !now (String.sub str i 1)
  done;
  inter !now maton.final_states <> []

let to_dfa = fun maton ->
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
  DFA.cons
    maton.alphabet
    (map
       (fun (s, c, s') ->
          let s = map (assoc_r maton.dictionary) s in
          let s' = map (assoc_r maton.dictionary) s' in
          (s, c, s'))
       !new_trans)
    (map (assoc_r maton.dictionary) new_init)
    (map (map (assoc_r maton.dictionary)) !new_finals)

let any = cons [] [(0, AnyOther, [0])] [0] [0] ~dictate:false
let just str = cons [str] [(0, Char str, [1])] [0] [1]

let string_of_char_like = function
  | Empty -> "ε"
  | AnyOther -> "other"
  | Char c -> c

let print_nfa maton string_of_state =
  let string_of_state state =
    try string_of_state (assoc state maton.dictionary)
    with Not_found -> string_of_int state in
  let print_a_to_b a c b =
    Printf.printf "\"%s\" -> \"%s\" [label = \"%s\"]\n"
      (string_of_state a) (string_of_state b) (string_of_char_like c) in
  Printf.printf "digraph finite_state_machine {\n";
  Printf.printf "rankdir=LR\n";
  Printf.printf "node [shape = point] init\n";
  Printf.printf "node [shape = ellipse, peripheries=2]\n";
  iter (fun s -> Printf.printf "\"%s\"" (string_of_state s)) maton.final_states;
  Printf.printf "\nnode [shape = ellipse, peripheries=1];\n";
  iter (fun s -> Printf.printf "init -> \"%s\"" (string_of_state s)) maton.initial_states;
  Printf.printf "\nnode [shape = ellipse, peripheries=1]\n";
  IntMap.iter
    (fun s cmap ->
       CharLikeMap.iter (fun c slist -> iter (print_a_to_b s c) slist) cmap)
    maton.transition;
  Printf.printf "}\n"
