module Comp = 
  struct 
    type t = int
    let compare  = (-)
  end

module M = Map.Make(Comp)
include M
