exception InvalidCharacter
type char_like = Empty | AnyOther | Char of string
type t

val cons : string list -> ('a * char_like * 'a list) list -> 'a list -> 'a list -> t
val run : t -> string -> bool
val to_dfa : t -> RegexDFA.t

val any : t
val just : string -> t
val repeat : t -> t
val concat : t list -> t

val print_nfa : t -> unit
