(** This module is for the delta transition function of automaton. One
can add elements, and search for transition.
*)
type 'state  t;;

(** Take a letter and give its successor, if none raise Not_found. O(d*log(size)*size)
*)
val successor :  Letter.t -> 'state t  -> 'state;;

(** Add a transition from a Letter.t to a state. O(d*log(size))
*)
val add : 'state t -> Letter.t -> 'state -> 'state t;;

(** An empty transition
*)
val empty:  'a t

(** Apply a function to each element of the transition. O(f()*size)
*)
val iter : (Letter.t -> 'state -> unit ) -> 'state t -> unit

(** similar than fold left. O(f()*size)
*)
val fold : (Letter.t -> 'state -> 'a -> 'a ) -> 'a -> 'state t -> 'a

(** same transition with a result applied on the state. O(f()*size)
*)
val map : ('state -> 'statebis ) -> 'state t -> 'statebis t

(** print the transition
*)
val printf : (Format.formatter -> 'state -> unit) -> Format.formatter ->  'state t -> unit

(* (\** take an association list of transition and return a 'state t*\)
*)
(* val from_list : (Letter.t * 'state) list -> 'state t
*)
