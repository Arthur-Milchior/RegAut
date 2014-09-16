module Comp : 
  sig 
    type t = int
    val compare : t -> t ->int
  end

module M : Map.S with type key = int
module S : Set.S with type elt = int

