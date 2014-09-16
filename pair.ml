module OrderedType(H1: Set.OrderedType) (H2: Set.OrderedType):
(Set.OrderedType with type t= (H1.t* H2.t))
  =
  struct
        type t = H1.t * H2.t
        let compare (a,c) (b,d)=
          let e = H1.compare a b in
          if e = 0 
          then H2.compare c d
          else e
  end

module Set(H1: Set.OrderedType) (H2: Set.OrderedType)
 = Set.Make (OrderedType(H1)(H2))

module Map(H1: Map.OrderedType) (H2: Map.OrderedType)
 = Map.Make (OrderedType(H1)(H2))
