type t

val cons : string list -> ('a * string * 'a) list -> 'a -> 'a list -> t
val run : t -> string -> bool
val minimize : t -> t
val print_dfa : t -> unit
