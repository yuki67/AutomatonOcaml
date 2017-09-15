exception InvalidCharacter
type char_like = Empty | AnyOther | Char of string
type 'a t

val cons :  ?dictate:bool -> string list -> ('a * char_like * 'a list) list -> 'a list -> 'a list -> 'a t
val run : 'a t -> string -> bool
val to_dfa : 'a t -> 'a list DFA.t

val any : int t
val just : string -> int t
val repeat : 'a t -> 'a t

val print_nfa : 'a t -> ('a -> string) -> unit
