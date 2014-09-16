(** 
  A simple automaton, with only letters and transition. We use it to translate to and from string.

The  "free line" are not read by the computer, they are for the human to use at his discretion

Description of a transition of dimension d:
2 line, The id of the following state, 
1 line, d lines containing an integer, 

Description of a state of dimension d:
1 lines, the description of each transition
2 lines, true or false, depending of if the state is final, 

Description of an automaton:
1 line
its basis b
its dimension d
then the description of each states
*)

type 
  (** 
The type of a state

op associate to each element its 0 orderered partition. 
delta is the transition function
automaton is its automaton
id an unique id
  
*)
  state = {  
  delta : int IntAr.M.t;
  id: int ;
  final : bool;
  nameS : string ; 
}

  (** The type of th automaton

  size is the number of states
  base is the basis in which the automaton is written
  dimension is the number of integer in a letter
  statesA is the set of states
  initial state is in position 0
*)
type  automaton = { base : int;
                    dimension : int ;
                    mutable states : state array ;
                    name: string;
                    natural : Math.set;
                    alphabet : IntAr.t list;
                    size : int;
                    minimal : bool;
                  }
type input = Aut of automaton | Mes of string

(** to value to initiate variable when needed
*)  
val state_dummy : state
val automaton_dummy : automaton

(** return true if the automaton is correct :
no integer greater than the basis, every array of dimension d, and every sucession to a state that exists. O(size * b^d)
*)
val check : automaton -> bool

(** generate a text for gnu dot to generate the automaton
*)
val printf_dot : Format.formatter -> automaton -> unit

(** generate a readable automaton
*)
val printf : Format.formatter -> automaton -> unit

(** if the automaton is in base 2 or 3, change it to base 4 or 9. O(size * (b*b)^d)
*)
val two_to_four : automaton -> automaton

(**add every missing transition to a garbage state, create it if
needed. O(size * b^d* log(b)*d)
 *)
val add_garb  : automaton -> automaton

(** remove non accessible state. O(size*b^d )
 *)
val accessible  : automaton -> automaton

(** minimize the automaton. O(size^2* b^d).
Probably smaller than size ^2; but I can't find the proof
*)
val minimize : automaton -> automaton

(** take a automaton A reading integers and return list of (N, A_N).
O(2^d*b^d*size* d*log(b)+ minimize())
*)
val zToN: automaton -> (Subset.t * automaton) list


(**
 apply two_to_four, add_garb, minimize, accessible, so we can begin the real work.
O(size^2*b^2d log(b)*d
*)
val correct: automaton -> automaton

(** return an array auts such that ar.(i) is the input with the ith
 position at 0.
O(size*b^{d+1}+ d*correct())
 *)
val remove_0 : automaton -> automaton array

(** an array accepting the input minus 1 on every position
O(size*2^d*b^d)
 *)
val remove_1 : automaton -> automaton

(** return true if the two automatons are equals when ignoring the name and order of the state.
O(size * b^d * log(size))
*)
val equal : automaton -> automaton -> bool

(** take a base, dimension, and size, and generate a random automaton with those caracteristic
O(size * b^d )
*)
val random : int -> int -> int -> Math.set -> automaton 
