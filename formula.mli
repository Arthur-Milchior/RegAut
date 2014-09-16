open Hashcons
(** The module that deals with formulas *)
type quantifier =
  | Free
  | Existential

(** The type of a variable, either free or bound (either int or bool) *)
type var 

(** return a new bounded variable, whose description is the input O(1)*)
val newBoundVar : string -> var

(** return an array of size d of variable whose description is the string and their pos in the array
O(d)
*)
val newBoundArray : int -> string -> var array

(** Return an array of variables whose size is the input. It always return variables equals for = but not for ==
O(d) *)
val free : int  -> var array

type t

(** O(1) *) 
val imply :( t* t )-> t
(** O(size*log(size)) *) 
val and_list : t list -> t
(** O(size*log(size)) *) 
val or_list : t list -> t
val true_ : t
val false_ : t
(** O(1) *) 
val not_ : t -> t
(** O(1) *) 
val bool : var -> t
(** O(1) *) 
val lessc : (var * int )-> t
(** O(1) *) 
val morec : (var * int )-> t
(** O(1) *) 
val const : (var * int )-> t
(** add (a,b,c) is a + b = c; O(1) *)
val add : (var * int * var) -> t
(** O(1) *) 
val minus : (var * int * var) -> t
(** O(1) *) 
val lessst: (var * var) -> t
(** a <= i +b *)
val less :  (var * int * var) -> t
(** O(1) *) 
val lesseq: (var * var) -> t
(** mod (x, i, k) is x = i mod k O(1)*)
val mod_ : (var* int * int) -> t
(** mode (x,i, y, k) is x +i = y mod k. O(1)*)
val mode : (var* int *var* int) -> t
(** assume a variable is strictly  negative. O(1) *)
val neg : var ->t

(** return a formula x+1=y. O(1)*)
val succ : (var * var) ->  t
(** return x=y. O(1)*)
val equal : (var *var) ->  t
(** return the equality between the tuple. O(d) *)
val eq_tuple : var array -> var array ->  t



(** "power bm f" apply Def 6.4. Multiply by b^m the constants. O(size) *)
val power: int -> t ->  t

(*
(** Takes a transition function from letters to formula, its basis
and generate the correct formula for the letters which are Letter *) 
val succ : int ->  t Transition.t ->  t*)

(** pretty print the variable *)
val printf_var : Format.formatter ->  var  -> unit

(** pretty print for the variable *)
val printf :  Format.formatter -> t  -> unit

(** take a formula, known to be true, to help simplification. O(1) *)
val assert_ : t-> t


(** an equivalent smaller formula assuming quantification over N. 
O(size^2). In practice, its probably really less, but I've no way to prove it. 
*)
val simplify_natural :  t ->  t

(** an equivalent smaller formula assuming quantification over Z. 
O(size^2). In practice, its probably really less, but I've no way to prove it. 
*)
val simplify_relative :  t ->  t

(** O(size^2). In practice, its probably really less, but I've no way to prove it.  *)
val simplify : Math.set -> t -> t

(** xs f change the free variable x_i by xs.(i) 
O(size)
*)
val change_free : var array ->  t ->  t

(** take a formula phi, a subset N, and generate phi_N. O(size) *)
val change_n: Subset.t -> t -> t

(** replace each "x" by "x-1". O(size) *)
val add1 : t -> t

(* move is when the zero are computed for FO[<] formula, to put the correct name to variables *)
(** Increment the variable j to j+1 when j>= i. O(size)*)
val move : int -> t -> t

(** an equivalent quantifier free formula, the first input is true if
over N. O(LONG)*)
val elim : Math.set -> t -> t

(** to_aut b d set f 
generate a simple automaton from a quantifier free formula whose base and dimension are the two first args. O(LONG) *)
val to_aut : int -> int -> Math.set ->t -> Simple.automaton

(** Taking a transition function to formula, generate a formula
according to this transition. O(\sum(size)) *)
val trans: int ->t Transition.t -> t

(** a lenght for the formula. O(size) *)
val length : t -> int

(* (\** random d m is a random formula of dimension d and integers less *)
(* than m. O(infiny) *\) *)
(* val random : Math.set -> int -> int -> t *)

(** random d m s
is a random formula of size s dimension d and integers less than m. O(infiny) *)
val random : Math.set -> int -> int -> int -> t

(** remove the assertion. O(size) *)
val remove_assert :  t -> t
(** Apply remove and then simplify.  
O(size^2)*)
val remove_simplify : Math.set ->  t -> t
