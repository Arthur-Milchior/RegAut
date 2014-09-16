(** A module for the letters *)

type t = Letter of IntAr.t
       | ParenT of (Subset.t * Subset.t) 
       | ParenD of Subset.t 
       (** ParenG B is ({},B) *)
       | ParenG of Subset.t  
       | ParenI of Subset.t 
       | CrochetT of (Subset.t * Subset.t) 
       | CrochetD of Subset.t 
       | CrochetI of (Subset.t * Subset.t)
       | ChevronT of (Subset.t * Subset.t) 
       | ChevronD of Subset.t 

type word = t list

(** "zero b d" generate the letter of dimension d in base b with zeros.
 O(1)
*)
val zero : int -> int -> t

(** print a letter 
*)
val printf : Format.formatter -> t -> unit


(** True if the base and the dimension is indeed d. O(d)
*)
val check_dim : int -> int -> t -> bool

(** int_to_word array b  generate a word in basis b writting the array. O(d* log(elements))
*)
val int_to_word : int array -> int -> word

module M : Map.S with type key = t

(** Fail on I, identity on Letter, return *D on everything else. O(1)
*)
val double : t -> t 
