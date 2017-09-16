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
type t = {
  alphabet : string list;
  transition : transition;
  initials : state list;
  finals : state list;
}

let get_alph x = x.alphabet
let get_trans x = x.transition
let get_inits x = x.initials
let get_finals x = x.finals

let lookup_nexts maton (states:state list) (char:char_like) =
  let next char state =
    let cmap =
      try IntMap.find state maton.transition
      with Not_found -> CharLikeMap.empty in
    try CharLikeMap.find char cmap with Not_found ->
    try CharLikeMap.find AnyOther cmap with Not_found -> [] in
  unions (map (next char) states)

let collect_states trans =
  fold_left (fun acc (s, c, slist) -> union (set_add slist s) acc) [] trans

let to_map triples =
  let assoc = fold_left (fun acc (x, y, z) -> (x, (y, z))::acc) [] triples in
  let collected = assoc_collect assoc in
  IntMap.from_alist
    (map (fun (s, alist) -> (s, CharLikeMap.from_alist alist)) collected)

let to_triples imap =
  let expand (state, cmap) =
    map (fun (c, slist) -> (state, c, slist)) (CharLikeMap.bindings cmap) in
  unions (map expand (IntMap.bindings imap))

let cons alph trans inits finals =
  {
    alphabet = alph;
    transition = to_map (map (fun (x, y, z) -> (hash x, y, map hash z)) trans);
    initials = map hash inits;
    finals = map hash finals;
  }

let saturate maton states =
  let lookup_nexts maton (states:state list) (char:char_like) =
    let next char state =
      try
        CharLikeMap.find char (IntMap.find state maton.transition)
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
  let now = ref (saturate maton maton.initials) in
  for i = 0 to String.length str - 1 do
    now := transit maton !now (String.sub str i 1)
  done;
  inter !now maton.finals <> []

let to_dfa = fun maton ->
  let new_init = (saturate maton maton.initials) in
  let new_finals =
    if anything_in_common new_init maton.finals
    then ref [new_init]
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
        new_finals :=
          union (filter (anything_in_common maton.finals) nexts) !new_finals;
        loop (set_add searched state) (union tl (diff nexts (state::searched)))
  in
  let _ = loop [] [new_init] in
  RegexDFA.cons
    maton.alphabet !new_trans new_init !new_finals

let trans_union t1 t2 =
  IntMap.union
    (fun _ cmap1 cmap2 ->
       Some (CharLikeMap.union (fun _ s1 s2 -> Some (union s1 s2)) cmap1 cmap2))
    t1 t2

let any = cons [] [(0, AnyOther, [1])] [0] [1]

let just str = cons [str] [(0, Char str, [1])] [0] [1]

let repeat maton =
  let new_trans =
    all_pairs
      (fun init final ->
         [(init, Empty, [final]); (final, Empty, [init])])
      maton.initials maton.finals
    |> unions
    |> to_map
    |> trans_union maton.transition in
  {
    alphabet = maton.alphabet;
    transition = new_trans;
    initials = maton.initials;
    finals = maton.finals;
  }

let map_over_state f maton =
  {
    alphabet = maton.alphabet;
    transition =
      fold_left
        (fun acc (x, cmap) ->
           IntMap.add (f x) (CharLikeMap.map (map f) cmap) acc)
        IntMap.empty (IntMap.bindings maton.transition);
    initials = map f maton.initials;
    finals = map f maton.finals;
  }

let hashi i maton =
  map_over_state
    (fun s -> hash (i, s))
    maton

let concat nfas =
  let a_to_b a b = (a, Empty, [b]) in
  let nfas = mapi hashi nfas in
  let new_trans =
    adjacent_pairs
      (fun prev next -> all_pairs a_to_b prev.finals next.initials)
      nfas
    |> unions
    |> to_map
    |> trans_union (fold_left trans_union IntMap.empty (map get_trans nfas)) in
  {
    alphabet = unions (map get_alph nfas);
    transition = new_trans;
    initials = (hd nfas).initials;
    finals = (last nfas).finals;
  }

let string_of_char_like = function
  | Empty -> "ε"
  | AnyOther -> "other"
  | Char c -> c

open Printf
let print_nfa maton =
  let string_of_state = string_of_int in
  let print_a_to_b a c b =
    printf "\"%s\" -> \"%s\" [label = \"%s\"]\n"
      (string_of_state a) (string_of_state b) (string_of_char_like c) in
  printf "digraph finite_state_machine {\n";
  printf "rankdir=LR\n";
  printf "node [shape = point] init\n";
  printf "node [shape = ellipse, peripheries=2]\n";
  iter
    (fun s -> printf "\"%s\"" (string_of_state s))
    maton.finals;
  printf "\nnode [shape = ellipse, peripheries=1];\n";
  iter
    (fun s -> printf "init -> \"%s\"" (string_of_state s))
    maton.initials;
  printf "\nnode [shape = ellipse, peripheries=1]\n";
  IntMap.iter
    (fun s cmap ->
       CharLikeMap.iter (fun c slist -> iter (print_a_to_b s c) slist) cmap)
    maton.transition;
  printf "}\n"
