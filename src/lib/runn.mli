module Runn : sig
  type t
  val generateRUNN: max: int -> density:int -> ?forceincludezero:bool -> ?forceexcludezero:bool -> unit -> t
  val getMinMax: runn:t -> (int option * int option)
  val toString: runn:t -> string
  val makeNumberLine : max:int -> t
  val sift: max:int -> numberline:t -> runn:t -> int
  (*todo: write RUNN to csv and accept csv as input so we can do time testing*)
end
                
