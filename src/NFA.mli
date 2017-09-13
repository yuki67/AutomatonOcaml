exception InvalidCharacter
type char_like = E | C of char
type 'state nfa

val cons : char list -> ('state * char_like * 'state list) list -> 'state list -> 'state list -> 'state nfa
val run : 'state nfa -> string -> bool
val to_dfa : 'state nfa -> 'state list DFA.dfa
val print_nfa : 'state nfa -> ('state -> string) -> unit
