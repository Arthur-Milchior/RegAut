(** the Module that do the real work over the automaton *)

(** indication to tell which set we search *)
type search = | Mod | Less
(** The type of a state *)
type state

(** The type of a Strongly Connected Component *)
type scc

(** The type of equivalence *)
type eq 

(** The type of an automaton *)
type automaton

module NotReg :
sig
              
  (* we can not directly use scc or state since that would create a
loop, so we give its the id of the state (of an element of the scc/eq) instead *)

  type t = 
    (** The automaton is not a relative set because of the initial state*)
    | Prop_rel_init
    (** The automaton is not a relative set because of the successor of the state by the letter*)
    | Prop_rel of (int * IntAr.t)
    (**(q) and its successor by 0 are not both final/not final *)
    | Prop00 of (state) 
    (** (q,l,q') delta(q,l)=q', q has a cycle, not q' *)
    | Prop01 of (state* Letter.t * state) 
    (** (q) if it has no state B*)
    | Prop02exists of state
    (** (q) if the intersection of the B that stay in the same scc does
  not stay in the same scc *)
    | Prop02array of (state )
    (** (q,q') are in the same scc, but B is different*)
    | Prop02state of (state * state)
    (** (q,s) if s is an intersection of some U, but not an U *) 
    | Prop03array of (state * Subset.t )
    (** (q,q') are in the same scc, but their U is different*)
    | Prop03state of (state * state)
    (** (q,u) u is a union of u_i, but does not respect the definition*)
    | Prop03union of (scc * Subset.t)
    (** (q, I,K) if the successor of q by (I,K) is not ({},K) cyclic *)
    | Prop04 of (state * Subset.t * Subset.t)
    (** (q, I,K) if the successor of q by [I,K] is not [I,K] cyclic *)
    | Prop05 of (state * Subset.t * Subset.t)
    (** (q,U) if there is q' in the same scc has q and their [U,B_C]
cycle have different size *)
    | Prop06 of (state* Subset.t)
    (** (q,U,B) if q in N(C,U,B) has a [U,B]-cycle not prime with b *)
    | Prop06Prime of (state* Subset.t* Subset.t)
    (** (s, us) if k_us does not divides the lcm of the k_u*)
    | Prop07 of (scc* Subset.t )
    (** (q,letter)  if letter makes q change z or b but incorrectly *)
    | Prop08 of (state * IntAr.t* state * state)
    (** (q,letter) if letter makes q does not change z or b and incorrectly *)
    | Prop09 of (state * IntAr.t* state * state)
    (** (q,N) if the set q does not respect the N hypothesis *)
    | Prop10 of (state*  Subset.t)
    (** (q,letter) if delta(q,letter) is not less than delta(q).letter *)
    | Prop11 of (state* Letter.t)
    (** (q,D,I) if the equality of 12 is not respected *)
    | Prop12 of (state* Subset.t* Subset.t)
    (** (q) if the equality of 13 is not respected *)
    | Prop13 of (state)
    (** (q,D,I,K) if the equality of 14 is not respected *)
    | Prop14 of (state* Subset.t* Subset.t* Subset.t)
    (** (q,D,I,K) if the equality of 15 is not respected *)
    | Prop15 of (state* Subset.t* Subset.t* Subset.t)
    (** (q,I) if equality 16 is not respected *)
    | Prop16 of (state * Subset.t)
    (** (q,i,j) if (i,j) is ignored, i<_q j and there are no k between them *)
    | Prop17 of (state* int*int) 
    (** This is not in the paper, but it helps to consider this case. 
    (q,u) if q is not accessible from delta(q,<U>) by <.> *)
    | Prop18bis of (state* Subset.t)
    (** (q,q') if q equiv q' and their oop are identic *)
    | Prop18 of (state * state )
    (** (q,i) if the equality of prop 19 is not respected*)
    | Prop19 of (state* Subset.t)
    (** (q,I) if the equality of 20 is not respected *)
    | Prop20 of (state* Subset.t)
    (** (q,I,K) if the equality of 21 is not respected *)
    | Prop21 of (state* Subset.t* Subset.t)
    (** (q,I,K) if the equality of 22 is not respected *)
    | Prop22 of (state* Subset.t* Subset.t)

  (**A number between 0 and 31 to indicate the property not respected *)
  val count : t -> int

  exception T of t
  val printf : Format.formatter -> t -> unit
end

(** This function takes a state and returns its oop if there is one*)
val state_oop : state-> Oop.t

(** return the name of the automaton *)
val get_name : automaton -> string

(** This function takes a state and returns its automaton *)
val state_automaton : state -> automaton

(** This function takes a state, a letter and return its successor *)
val successor : state -> Letter.t ->  state


(** return the base of the automaton *)
val aut_base : automaton -> int 

(** return the dimension of the automaton *)
val aut_dimension : automaton -> int 

(** return the array of states *)
val aut_states : automaton -> state array

(** Generate an automaton from a Simple automaton. Ready to test existence *)
val of_simple : Simple.automaton -> automaton

(** Return None if a FO[<,mod] exists, else the element that makes it
false *)
val exists : search -> automaton -> NotReg.t option;;
(** Translate a simple automaton into an automaton, with a n class every component corrects
 *)

val to_simple : automaton -> Simple.automaton;;

(** print equality-class information*)
val eq_printf  : Format.formatter -> eq -> unit

(** print scc information*)
val scc_printf  : Format.formatter -> scc -> unit

(** print the description of an automaton *)
val aut_printf : Format.formatter -> automaton -> unit

(** same as aut_printf *)
val printf : Format.formatter -> automaton -> unit

(** print a transition, mostly used for debugging *)
val trans_printf : Format.formatter -> state Transition.t -> unit

(** print the description of a state *)
val state_printf : Format.formatter -> state -> unit


(** construct the formula *)
val construct :Formula.quantifier -> automaton -> Formula.t

(** check that property 11 is verified *)
val checkN : Subset.t -> automaton -> unit



exception N of (NotReg.t * automaton )
exception E of (exn * automaton)

type return = 
  | Formula of Formula.t
  | Error of (automaton * NotReg.t)

val construct_simple: search -> Formula.quantifier -> Simple.automaton -> return
