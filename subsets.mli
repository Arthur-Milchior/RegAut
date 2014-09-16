(** Union of disjoint non empty subsets of [0,d-1]. equality is testable*)
type t

(** the empty set of size d. O(d)*)
val empty : int -> t

(** a list of the union of the elements of t. O(2^size) *)
val enumerate : t -> (t list)

(** a list of the partition of the elements of t. O(2^size) *)
val partitions : t -> (t*t) list


(** "inters us s" takes the intersection of s with every element of us. add the remaining O(size*log(size)) *)
val inters  : t -> Subset.t -> t

(** "add us s" put s in the subsets, s is assumed to be disjoint of the element of us. O(log(size)) *)
val add  : t -> Subset.t -> t

(** true if there is no set. O(1) *)
val is_empty : t-> bool

(** Take a list of subset, a dimension, and generate a subsets from it.
In general O(2^size), but in fact O(d* size)  *)
val cut : Subset.t list -> int -> t

(** The union of subsets as set. O(1) *)
val union : t -> Subset.t 

(** apply a function to each class, such that the least element increase.
O(size* f())
 *)
val iter : (Subset.t -> unit) ->t -> unit

(** is a set an element of the set of set.
 O(log(size))*)
val inS : Subset.t -> t -> bool

(** print the set of subset *)
val printf : Format.formatter -> t -> unit

(** the subset a list in order according to subset.compare_int. O(size) *)
val list :  t -> Subset.t list

(** take a subsets, a list of subset of the subsets, unify the element of the list. 
 O(list size * log(set size)*)
val unify : t -> Subset.t list -> t

(* (\** return the number of subset *\) *)
(* val nb : t-> int *)
