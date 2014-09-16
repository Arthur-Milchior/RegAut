(** the type of an int array 
*)
type t
(** Generate the list of all d tuples of numbers between 0 and b-1; O(b^d) 
*)
val enum : int -> int -> t list

(** Generate the list of all d tuples of numbers between 1 and b-2 O((b-2)^d)
*)
val enum_non_r : int -> int -> t list

(** take the base, the dimension, an array, return it as t; the base is used for classification value. O(d) 
*)
val make : int -> int -> int array -> t 

(** generate 0d O(1)*)
val zero : int -> int-> t
(** print a letter 
*)
val printf : Format.formatter -> t -> unit



module  M:  Map.S with type key = t
module S : Set.S with type elt = t

(** get t i return the ith element. O(1) *)
val get : t -> int -> int

(** take the base, the dimension, two subset s1 s2 of this dimension and return (s1,s2). O(d)  
*)
val of_par2 : int-> int -> Subset.t ->Subset.t -> t

(** take the base, the dimension, a subset s1 of this dimension and return (s1). O(d) 
*)
val of_par : int-> int -> Subset.t -> t

(** return an identic copy O(1)*)
val copy : t -> t

(** set i v t, t where i is v O(log(i)+d)
*)
val set : t->int-> int-> t

(** a copy of t where every position in the set is decremented. O(d)
*)
val decr : t -> Subset.t -> t

(** a copy of t where every position in the set is incremented, and the set of overflow. O(d)
*)
val incr : t -> Subset.t -> (t*  Subset.t)

(** return the array. O(1)
*)
val to_ar : t -> int array

(** the dimension of the array. O(1)
*)
val dimension : t -> int

(** the concatenation of two array. O(d)
*)
val concat : t -> t-> t

(** inv t n b-complement the element of n in t. O(d)
*)
val inv : t -> Subset.t -> t 

(** "remove i t" remove the ith element of t. O(d)
*)
val remove : int -> t -> t

(** "check_dim b d t" verify if each position as at most b-1 and the array if of size d; O(d) *)
val check_dim : int -> int-> t -> bool

(** O(1) *)
val compare : t -> t -> int
