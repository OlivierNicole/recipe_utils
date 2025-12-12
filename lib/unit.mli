type t

val name : t -> string

val none : t
val g : t
val ml : t

val of_string_exn : string -> t

val pp : Format.formatter -> t -> unit
val pp_quantity : quantity:Fractional.t -> Format.formatter -> t -> unit

val equal : t -> t -> bool
val compare : t -> t -> int

val add :
  Fractional.t -> t -> Fractional.t -> t -> Fractional.t * t
