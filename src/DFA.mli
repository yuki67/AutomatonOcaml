type 'state t

val cons : string list -> ('state * string * 'state) list -> 'state -> 'state list -> 'state t
val run : 'state t -> string -> bool
val minimize : 'state t -> 'state list t
val print_dfa : 'state t -> ('state -> string) -> unit
