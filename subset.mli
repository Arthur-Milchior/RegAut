(** Subset of [0,d-1], for practical reason, d<= 30
*)

type t_node 
type t = t_node Hashcons.hash_consed

(** take a set, and return the list of its subsets;
 let i the size of the set; O(2^i)
*)
val enumerate_sub : t ->  t list
(** enumerate d: list  of the subsets of [0,d-1]  O(2^d)
*)
val enumerate : int -> t list
(** the empty set of size d O(1)
*)
val empty : int -> t
(** the complete set of size d O(1)
*)
val full : int -> t

(** the value of the subset as a binary number. O(1) 
*)
val value : t -> int 
(** the reverse function, taking b as first argument. O(1) 
*)
val binary : int -> int -> t

(* a type used for comparison 
*)
(** a sorting function, the bigger have the higher element. O(1)
*)
val compare : t -> t -> int
(** true if the set is empty. O(1)
*)
val is_empty : t -> bool
(** sub set1 set2 if set1 is a subset of set2. O(1) 
*)
val is_subset : t -> t-> bool
(** intersection of the subset. O(1) 
*)
val inter : t -> t -> t
(** union of the subset. O(1) 
*)
val union : t -> t -> t
(**complement of a set. O(1) 
*)
val comp : t -> t

(** t1 minus t2. O(min) 
*)
val minus : t -> t -> t

(** the number of element of t. O(size) 
*)
val count : t -> int

(** "ins set i" is true if i is in the set. O(1) 
*)
val ins : t -> int -> bool

(** true if the set are disjoint. O(1) 
*)
val is_disjoint : t -> t-> bool

(** Apply the function to the position in the set. O(d) 
*)
val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

(** Apply the function to the position in the set. O(d)
*)
val iter : (int -> unit) -> t -> unit

(** "add t pos" add the position pos to t, it change the subset. O(1)
*)
val add : t -> int -> t

(** returns the set with element only in left, in center, and in
right. O(1)
*)
val cut_base : t -> t -> (t * t * t)

(** print the subset
*)
val printf : Format.formatter -> t -> unit

(** true if the dimension is the argument. O(1)
*)
val check_dim : int -> t -> bool

(** return any element of the set, fail if the set is empty. O(min)
*)
val elt : t -> int

(** an identic but distinct copy of the op O(1)
*)
val clone : t -> t 


(** the dimension of the set. O(1)
*)
val dimension : t-> int
