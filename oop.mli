(** Comparaison element between two position *)
type comp =  Les | Eq | Gre | Bot ;;

(** A 0 ordered partition *)
type  t = {size : int ;
           mutable z : Subset.t;
           mutable b: Subset.t;
           mutable u : Subsets.t ;
           comparison: comp array array}


(** Initialize an oop of size d with least possible values.
O(d^2)
*)
val init : int -> t

(** less o1 o2 is true if o1 is less or equal o2.
O(d^2)
 *)
val less_eq : t -> t -> bool

(** Take an element, two position i j, and a comparison.  Assign this
element at position i j to the greatest lower than its current value
and the comparison. Also work on j i. 
O(1) *)
val lower : t-> int -> int -> comp -> unit

(** verify if the OP respects the condition on N  for /N language.
O(d^2)
*)
val check : t -> Subset.t -> bool

(** takes an oop, the basis, and a letter as a d-tuple, and concatenate them.
O(d^2)
*)
val concat : t -> int -> Letter.t -> t

(** Print the whole structure 
*)
val printf : Format.formatter -> t -> unit

(** gen_ex oop z b generate a word in base b which is an example of this oop. 
O(d log(bd))
*)
val gen_ex : t -> Subset.t -> int -> Letter.word
(*
(** take an oop, for each class is< js, generate a oop equal to the
input, with is=js, and from it, return is,js and a word that respect this op
*)
val pair_union : t  -> (Subset.t * Subset.t * Letter.word ) list
 
*)
(** takes two class is < js and return a new oop which make their union.
O(d^2)
*)
val unite : t -> Subset.t -> Subset.t -> t


(** Return the formula that accepts the set as designed in the op, on /z 
O(d^2)
*)
val formula_eq : t -> Subset.t -> Formula.var array ->  Formula.t

(** true if the int is in a u class of the op.
O(1)
*)
val is_u : t -> int -> bool
module  M:  Map.S with type key = t
module  S:  Set.S with type elt = t

(** return true if the same position are bottom in both oop *)
val bot_eq : t -> t -> bool

(** return true if the two oop are incomparable *)
val incomparable : t -> t -> bool

(** a comparison function *)
val compare : t -> t -> int
