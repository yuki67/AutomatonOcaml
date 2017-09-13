type 'state dfa
exception InvalidCharacter

val cons : char list -> ('state * char * 'state) list -> 'state -> 'state list -> 'state dfa
val run : 'state dfa -> string -> bool
val minimize : 'state dfa -> 'state list dfa
val print_dfa : 'state dfa -> ('state -> string) -> unit
