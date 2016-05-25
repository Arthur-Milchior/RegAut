let id_aut = ref (-1)

type search = | Mod | Less
open Format
let print_debug _ = ()
(* let print_debug x = print_endline x *)

(** 
  We must simultaneously define the state, the strongly connected component (scc) and the automaton, since each link to each other. We also provide some function on it.
 *)

module U2M = Pair.Map (Subset)(Subset)
module UHmap = Pair.OrderedType (Subset)(Subset)
module UUUHmap = Pair.OrderedType (UHmap)(UHmap)
module U5M = Pair.Map (Subset)(UUUHmap)
(* Input of type (i, (d1l,d2l), (d1g,d2g)) *)

type iota = Ignore | Important

module rec 
    Type: 
      sig
        type 
          (**
the type of a state 
     id an unique id
     final is true if the state is final
     delta is the transition function.

     automaton is its automaton.
     scc gives its strongly connected compontent, a dummy one if there is no cycle.
     oop associate to each element its 0 orderered partition. A dummy one if there are no cycle.
     eq its equivalence class for ~

     visited and visted' are variable used by many function to temporary mark the states, the automaton must be unmarked at the end of the loop.

     step[i] is true if there is a path of size i from the initial state with acyclic state
     kappa[q][q']  correspond to kapa_q(q') of the paper, that is the lexicographicaly minimal words from q to q' if it exists
     zeros: a function that takes a scc and return a state in it if it exists

     rel : a function from i,d1<, d2<,d1>,d2> to the set of states equivalent to it
     usable : true if there is no Z and no B.
           *)
          state = { id: int ;
                    final : bool ;
                    mutable delta : state Transition.t;
                    nameS : string ; 

                    automaton : automaton ;
                    mutable scc : scc ;
                    mutable oop : Oop.t;
                    mutable eq : eq; 

                    mutable cyclic : bool;
                    mutable visited :bool;
                    mutable visited' : bool;

                    mutable step : Int.S.t;
                    mutable kappa : int array SM.t ;
                    mutable zeros : state CM.t;

                    mutable formula :  Formula.t option;
                    (*i, d1l, d2l, d1g, d2g*)
                    mutable rel :bool array U5M.t;
                  }
        and
          (** 
The type of a strongly connected component
          idS an unique id
          statesS the list of its states, in the same order as in the automaton array
          op its 0 partition

          ku is a function from U unions of U_{C,i} to the size of the [U]-loop
          k is the lcm of the ku

          If the B-class is empty:

          eqs : its set of equivalence class; a parition of statesS if the B class is empty, else nil
          rewritting : a rule of rewritting rule to apply, in the order of Subsets.t, hence they must be applied in reverse order
            a rule is (U,(i, is)) if U^i is equivalent to is
           *)
          
          scc = { mutable idS : int;
                  mutable statesS : state list;
                  a_state : state;

                  mutable ku : (Subset.t_node,int)  Hmap.t;
                  mutable k : int;
                  
                  mutable seen : bool;

                  mutable eqs : eq list;
                  mutable rewriting : (Subset.t_node,(int * int array)) Hmap.t
                } 
        and
          (** 
         The type of an equivalence class according to ~

        idE an unique id
        statesE the set of states
           (*sccE a function from the scc to its state in  scc inter eq*)
           *)     
          eq = { idE : int;
                 mutable statesE : State_oop_set.t;
                 iota : iota array array;
               } 

        and
          
          (** The type of the automaton

    name : a description
    idA : an unique id

    size is the number of states
    base is the basis in which the automaton is written
    dimension is the number of integer in a letter
    var the set of free variables

    states the set of state, 0 is initial, otherwise no order
    sccs is the array of scc in topological order; the first has no successor, the last no predecessor   
    cycles the states with a cycle
    b_less the states whose b class is empty
    b_less_scc their scc

    use_suc if +1 is needed
    use_less if < is needed
    use_zero if =0 is needed
    use_equal if = is needed

           *)
          automaton = 
          { name: string ;
            idA : int ; 
            simple : Simple.automaton ;
            alphabet : IntAr.t list;

            size : int ;
            base : int;
            dimension : int ;
            var : Formula.var array ;


            states : state array;
            mutable sccs : scc array;
            mutable cycles : state list;
            mutable b_less : state list;
            mutable b_less_scc : scc list;


            mutable use_suc : bool;
            mutable use_less: bool;
            mutable use_zero : bool;
            mutable use_equal: bool;
            mutable use_mod: bool;
          }
      end
      = 
        struct
          type 
          state = { id: int ;
                    final : bool ;
                    mutable delta : state Transition.t;
                    nameS : string ; 

                    automaton : automaton ;
                    mutable scc : scc ;
                    mutable oop : Oop.t;
                    mutable eq : eq; 

                    mutable cyclic : bool;
                    mutable visited :bool;
                    mutable visited' : bool;

                    mutable step : Int.S.t;
                    mutable kappa : int array SM.t ;
                    mutable zeros : state CM.t;

                    mutable formula :  Formula.t option;

                    mutable rel :bool array U5M.t;
                  } 
          and
          scc = { mutable idS : int;
                  mutable statesS : state list;
                  a_state : state;

                  mutable ku : (Subset.t_node,int)  Hmap.t;
                  mutable k : int;
                  
                  mutable seen : bool;

                  mutable eqs : eq list;
                  mutable rewriting : (Subset.t_node,(int * int array)) Hmap.t
                } 
          and
          eq = { idE : int;
                 mutable statesE : State_oop_set.t;
                 iota : iota array array;
               } 
          and
          automaton = 
          { name: string ;
            idA : int ; 
            simple : Simple.automaton ;
            alphabet : IntAr.t list;
            size : int ;
            base : int;
            dimension : int ;
            var : Formula.var array ;


            states : state array;
            mutable sccs : scc array;
            mutable cycles : state list;
            mutable b_less : state list;
            mutable b_less_scc : scc list;


            mutable use_suc : bool;
            mutable use_less: bool;
            mutable use_zero : bool;
            mutable use_equal: bool;
            mutable use_mod: bool;
          }
        end
    and 
       CompS : sig 
                 type t = Type.state 
                 val compare : t -> t -> int 
               end
      = struct
      type t = Type.state
      let compare {Type.id=x} {Type.id=y} = x-y
    end
    and
      SM : Map.S with type key = Type.state = Map.Make (CompS)
    and
      SS : Set.S with type elt = Type.state  = Set.Make (CompS)
    and
      State_oop_comp : 
        sig 
          type t = Type.state
          val compare : t -> t-> int
        end
      =
      struct 
        type t= Type.state
        let compare s t = Oop.compare s.Type.oop t.Type.oop
      end
    and
      State_oop_set :
        Set.S with type elt = Type.state
                              = Set.Make (State_oop_comp)
    and
      CompE : sig 
                type t = Type.eq
                val compare : t -> t -> int 
              end
      = 
      struct
        type t = Type.eq
        let compare {Type.idE=x} {Type.idE=y} = x-y
      end
    and
      CompC  : sig
                 type t = Type.scc
                 val compare : t -> t -> int 
               end
      = 
      struct
        type t = Type.scc
        let compare {Type.idS=x} {Type.idS=y} = x-y
      end
    and
      CM: Map.S with type key = Type.scc = Map.Make (CompC)
                                                    
include Type


let state_print_id ppf state = fprintf ppf "%d" state.id
let state_print_name ppf state = fprintf ppf "%s" state.nameS
let scc_print_id ppf state = fprintf ppf "%d" state.idS

let trans_printf =
  Transition.printf state_print_id

let eq_printf ppf eq =
  fprintf ppf "eq of id:%d, and states: " eq.idE ;
  State_oop_set.iter (fun state -> fprintf ppf "%a, " state_print_id state) eq.statesE


let scc_printf ppf scc =
  fprintf ppf "scc of id:%d,@  k:%d@,;" scc.idS  scc.k ;
  Pretty.print_list ppf  "ku=@[{" ",@," "}@]" (Hmap.bindings scc.ku) (fun ppf (u,ku) -> fprintf ppf "(%a,%d)" Subset.printf u ku);
  Pretty.print_list ppf  ",@,states:@[[" "," "]@]"  scc.statesS state_print_id;
  fprintf ppf " and eq : ";
  Pretty.print_list ppf "@[ <" ";@," ">@]@." scc.eqs eq_printf


(*a list of getter *)

(** O(1) *)
let get_name {name = n} = n
(** O(1) *)
let state_oop {oop=oop}= oop
(** O(1) *)
let state_scc {scc=scc}= scc
(** O(1) *)
let state_automaton {automaton= aut}  = aut

(** O(d* size*log(size))*)
let successor state letter =  
  Transition.successor letter state.delta 

(** take a word as a list of letter, return the successor by this
word; O(d* size*log(size)*list zie) *)
let successors  = 
  List.fold_left successor 

(** add a transition. O(d*log(size)) *)
let add_transition state letter suc =
  state.delta <- Transition.add state.delta letter suc

module NotReg =
struct
(*  type state = state
  type scc = int
  type eq = int*)
              
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
    
  exception T of t
                   
  let printf ppf p= 
    (match p with
     | Prop_rel_init ->
        fprintf ppf "The automaton is not even a correct relative one, its initial state is accepting.@."
     | Prop_rel (i, a) ->
        fprintf ppf "The automaton is not even a correct relative one, the successor of state %d by %a is accepting.@." i IntAr.printf a
     | Prop00 (state) ->
        fprintf ppf "The automaton is not even correct, as only one between state %s and its successor by 0...0 is accepting.@." state.nameS
     | Prop01 (state, letter, succ) ->
        fprintf ppf "Property 1 is not respected, as state %s,@, has a cycle and the letter " state.nameS;
        Letter.printf ppf letter;
        fprintf ppf " leads to %s which has no cycle" state.nameS
     | Prop02exists state->
        fprintf ppf " Property 2exists"
     | Prop02array (state )->
        fprintf ppf " Property 2array: the intersection of the Uc of %s is not in Uc " state.nameS
     | Prop02state (state, state2)->
        fprintf ppf " Property 2state: %s's b-class is %a and %s's is %a" state.nameS Subset.printf state.oop.Oop.b  state2.nameS Subset.printf  state2.oop.Oop.b
     | Prop03array (state, u )->
        fprintf ppf "Prop03 is false for state %s, with u=%a" state.nameS Subset.printf u
     | Prop03state (state , state2)->
        fprintf ppf " Property 3state"
     | Prop03union (scc , sub)->
        fprintf ppf " Property 3union"
     | Prop04 (state, i,k)->
        fprintf ppf " Property 4: delta(%s,(%a,%a)) is not in a ({},%a)-cycle" state.nameS Subset.printf i Subset.printf k Subset.printf k
     | Prop05 (state, i,k)->
        fprintf ppf " Property 5: delta(%s,[%a,%a]) is not in a [%a,%a]-cycle" state.nameS Subset.printf i Subset.printf k Subset.printf i Subset.printf k
     | Prop06 (state2, u)->
        fprintf ppf " Property6 is not respected, for u=%a, the loop on states ? and %s are different" Subset.printf u state2.nameS
     | Prop06Prime (state, u, b)->
        fprintf ppf " Property 6 prime"
     | Prop07 (scc, us )->
        fprintf ppf " Property 07"
     | Prop08 (state , ints, left, right)->
        fprintf ppf " Property 08 is not respected, delta(%s,%a)=%s and should be %s" state.nameS 
                IntAr.printf ints
                left.nameS right.nameS
     | Prop09 (state , ints, left, right)->
        fprintf ppf " Property 09 is not respected, delta(%s,%a)=%s and should be %s" state.nameS 
                IntAr.printf   ints
                left.nameS right.nameS
     | Prop10 (s,n )->
        fprintf ppf " Property 10"
     | Prop11 (state, letter)->
        let suc = successor state letter in
        (** (q,letter) if delta(q,letter) is not less than delta(q).letter *)
        fprintf ppf " Property 11: state q=%s has op @.%a, its successor @.%s by @.%a has op @.%a which is not less than the concatenation @.%a." 
                state.nameS Oop.printf state.oop  suc.nameS Letter.printf letter Oop.printf suc.oop Oop.printf (Oop.concat state.oop state.automaton.dimension  letter)
     | Prop12 (state, d, i)->
        fprintf ppf " Property 12"
     | Prop13 (state)->
        fprintf ppf " Property 13"
     | Prop14 (state, d,i,k)->
        fprintf ppf " Property 14: delta(%s,[%a,%a][%a]) is not equal to delta(%s,[%a][%a]<%a cup %a>)" state.nameS Subset.printf k Subset.printf d Subset.printf i state.nameS Subset.printf i Subset.printf k Subset.printf i Subset.printf d
     | Prop15 (state, d, i,k)->
        fprintf ppf " Property 15"
     | Prop16 (state, i)->
        fprintf ppf " Property 16: for I=%a, delta(%s,[I])=%s, delta(%s,<I>[I])=%s, delta(%s,[I]<I>)=%s should be identic" Subset.printf i
                state.nameS (successor state (Letter.CrochetD i)).nameS
                state.nameS (successor (successor state (Letter.ChevronD i)) (Letter.CrochetD i)).nameS
                state.nameS (successor (successor state (Letter.CrochetD i)) (Letter.ChevronD i)).nameS
     | Prop17 (state, i,j)->
        fprintf ppf " Property 17"
     | Prop18 (q,q')->
        fprintf ppf " Property 18: %a's op:@.%a@. and %a's op:@.%a@.are comparable" state_print_name  q Oop.printf q.oop  state_print_name q' Oop.printf q'.oop
     | Prop18bis (state, u)->
        fprintf ppf " Property 18bis"
     | Prop19 (state, i)->
        fprintf ppf " Property 19"
     | Prop20 (state, i)->
        fprintf ppf " Property 20"
     | Prop21 (state, i,k)->
        fprintf ppf " Property 21"
     | Prop22 (state, i,k)->
        fprintf ppf " Property 22");
    fprintf ppf "@."

    let count = function
     | Prop_rel_init -> 30
     | Prop_rel _ -> 31
     | Prop00 _ -> 0
     | Prop01 _ -> 1
     | Prop02exists state-> 2
     | Prop02array _-> 2
     | Prop02state _->2
     | Prop03array _->3
     | Prop03state _->3
     | Prop03union _->3
     | Prop04 _->4
     | Prop05 _ -> 5
     | Prop06Prime _
     | Prop06 _ -> 6
     | Prop07 _-> 7
     | Prop08 _ -> 8
     | Prop09 _ -> 9
     | Prop10 _ -> 10
     | Prop11 _-> 11
     | Prop12 _-> 12
     | Prop13  _-> 13
     | Prop14 _-> 14
     | Prop15 _-> 15
     | Prop16 _-> 16
     | Prop17 _-> 17
     | Prop18 _-> 18
     | Prop19 _-> 19
     | Prop18bis _-> 18
     | Prop20 _ ->  20
     | Prop21 _-> 21
     | Prop22 _-> 22
end


  
let aut_base {base =base} = base
let aut_dimension {dimension =dimension} = dimension
let aut_states {states =states} = states


let rec state_dummy =
  {oop= Oop.init 0;
   scc= scc_dummy;
   nameS= "dummy" ;
   zeros =CM.empty;
   step =  Int.S.empty;
   formula = None;
   kappa = SM.empty;
   delta= Transition.empty ;
   id = -1;
   automaton = automaton_dummy ;
   final = false;
   eq=eq_dummy;
   visited=false;
   visited'=false;
   cyclic =false; 
   rel = U5M.empty;}
and scc_dummy =
  { idS = -1;
    a_state = state_dummy;
    rewriting  = Hmap.empty;
    ku =Hmap.empty;
    k=1;
    eqs =[];
    statesS=[]; 
    seen = false;
  }
and eq_dummy = 
  {idE = -1;
   statesE = State_oop_set.empty;
   iota = [||];
  }
and automaton_dummy = 
  {idA= -1;
   simple = Simple.automaton_dummy;
   alphabet = [];
   size = 0;
   base = 0;
   name = "";
   b_less = [];
   var = [||];
   b_less_scc = [];
   dimension = 0;
   sccs = [||] ;
   states = [||];
   cycles = [];
   use_suc = false;
   use_less = false;
   use_zero = false;
   use_equal = false;
   use_mod = false;
  }


let state_printf ppf state =
  fprintf ppf "@[%s state q%d=%s@, scc: %d, eq: %d, %s cycle, transitions:@ %a@,,  its 0op is:%a@, its formula is %a@]"
          (if state.final then "accepting" else "rejecting") 
          state.id 
          state.nameS
          state.scc.idS  state.eq.idE
          (if state.cyclic then "a" else "no")
          (Transition.printf (fun ppf state -> fprintf ppf "%d" state.id)) state.delta
          Oop.printf state.oop
          (Pretty.some Formula.printf) state.formula
          (* rel_printf state *)


let aut_printf ppf aut =
  fprintf ppf "Automaton %s:@ Base: %d; Dimension: %d; size: %d;using %s%s%s%s%s;.@ "
          aut.name aut.base aut.dimension aut.size 
          (if aut.use_suc then "+1 "else "")
          (if aut.use_less then "< "else "")
          (if aut.use_zero then "=0 "else "")
          (if aut.use_equal then "= "else "")
          (if aut.use_mod then "mod "else "");
  Pretty.print_array ppf "@[ <" ";@." ">@]" aut.states
                     (fun ppf _ elt ->
                      state_printf ppf elt);
  fprintf ppf " and scc :@. ";
  Pretty.print_array ppf "@[ <" ";@." ">@]" aut.sccs (fun ppf _ s ->  scc_printf ppf s);
  fprintf ppf "@."

let printf = aut_printf

(** Translate a simple automaton into an automaton.
O(size*b^d*log (b) * d )
*)

let of_simple simple=
  let simple = Simple.correct simple in
  let b = simple.Simple.base 
  and d = simple.Simple.dimension 
  and ss = simple.Simple.states 
  and name = simple.Simple.name in
  let abd = simple.Simple.alphabet in
  let size = Array.length ss in
  let x = Formula.free d in
  let states = Array.make size state_dummy  in
  let rec automaton =
    {automaton_dummy with simple = simple;
                          size = size ;
                          base = b;
                          name = name;
                          dimension = d;
                          states = states;
                          var = x ;
                          alphabet = abd ;
    }
  in 
  (* putting states in the array *)
  for i=0 to (size -1) do
    states.(i) <-
      {state_dummy with id = i;
                        oop = Oop.init d;
                        automaton = automaton ;
                        final = ss.(i).Simple.final;
                        nameS= ss.(i).Simple.nameS ;
      }
  done;
  (* adding transition for each letter *)
  for i=0 to size -1 do
    IntAr.M.iter
      (fun letter suc ->
       let letter = (Letter.Letter letter) in
       add_transition states.(i) letter states.(suc)
      ) ss.(i).Simple.delta
  done;
  automaton

exception N of (NotReg.t * automaton )
exception E of (exn * automaton)
module Exists:
sig 
  val exists : search -> automaton -> NotReg.t option 
end =
struct

              
  (** Verify that when adding a 0 the state remain (not) final*)
  let check0 aut = 
    let zero = Letter.zero aut.base aut.dimension in
    Array.iter 
      (fun state ->
       let suc = successor state zero in
       if suc.final <> state.final 
       then raise (NotReg.T (NotReg.Prop00 (state)))
      ) aut.states


  (** Takes an automaton without sccs, add its sccs using Tarjan
algorithm. Not neccessary in any order *)
  let tarjan aut =
    let sccs = ref [] in
    let size = aut.size in 
    let lowLink = Array.make size None in
    let tar_index = Array.make size (-1) in
    let cur_index = ref 0 (* Number of seen state *)
    and cur_scc = ref 0 (*Number of scc *)
    and s = ref []  in (* A FILO with the visited states not yet in an scc. *)
    let rec aux state = 
      if tar_index.(state.id) = -1 
      then
        (tar_index.(state.id)<- !cur_index;
         incr cur_index; 
         s := state:: !s;
         state.visited <- true;
         Transition.iter         
           (fun _ suc ->
            if tar_index.(suc.id) = -1 
            then( (*if this successor has never been visited yet *)
              aux suc;
              lowLink.(state.id) <- 
                (match (lowLink.(state.id), lowLink.(suc.id)) with
                 | None, None -> None (*If neither us nor the successor has cycle *)
                 | None, Some i -> if i <= tar_index.(state.id) then Some i else None
                 | Some i, None-> Some i(*if one of us  have a cycle *)
                 | Some i, Some j -> Some (if i<j then i else j)) (*If both have a cycle *)
            ) else 
              if suc.visited  (* If the successor is also an ancester *)
              then lowLink.(state.id) <-
                     Some (match (lowLink.(state.id), tar_index.(suc.id)) with
                           | None, i -> i
                           | Some i,j -> if i<j then i else j)
           ) state.delta ;
         match lowLink.(state.id) with 
         | None -> (* If we have seen no path to an ancestor; hence no cycle *)
            s := List.tl !s
         | Some i when i = tar_index.(state.id)  -> (* if we have seen a path to ourself and none to an ancestor*)
            let fin = ref false in
            let scc =  {scc_dummy with  idS = !cur_scc; a_state = state} in
            incr cur_scc;
            sccs := scc :: !sccs;
            while not !fin do
              match !s with
              | [] -> 
                 failwith "We should never see the end of the list without seeing state as state2"
              | state2::t->
                 s := t;
                 state2.scc <- scc ;
                 state2.cyclic <- true;
                 if state2 == state then fin := true;
                 aut.cycles <- state2 :: aut.cycles;
                 scc.statesS <- state2 :: scc.statesS
            done ;
         | _ -> ()(* If there is a path to an ancester, there is nothing to do*)
        ); (*end of the if *)
      state.visited <- false
    in
    Array.iter aux aut.states;
    aut.sccs <- Array.of_list ( !sccs)
      
  (** Verify property1, after Tarjan algorithm is done, so state.cycle
are correct *)
  let prop01 aut =
    List.iter (
        fun state->
        Transition.iter 
          (fun letter suc ->
           if not suc.cyclic then
             raise (NotReg.T (NotReg.Prop01 (state,letter, suc)))
          )state.delta; 
      ) aut.cycles
               
  (** add every successor of the form (U,B) and (U)*)
  let addParen aut=
    List.iter (* looping on each U*)
      (fun u  ->
       List.iter (* looping on each B*)
         (fun b -> 
          List.iter  (* looping on each state with a cycle *)
            (fun state->
             let letter= IntAr.of_par2 aut.base aut.dimension u b in
             let parenthesis = Letter.ParenT (u,b) in
             let letter = Letter.Letter letter in 
             let succ = successor state letter in
             add_transition state parenthesis succ;
             if Subset.is_empty b
             then
               (let parenthesis = Letter.ParenD u in
                add_transition state parenthesis succ);
             if Subset.is_empty u
             then
               (let parenthesis = Letter.ParenG b in
                add_transition state parenthesis succ)
            ) aut.cycles
         ) (Subset.enumerate_sub (Subset.comp u)) ;
      )(Subset.enumerate aut.dimension)
              
  (** Check property 2, after the parenthesis were added; computes the B *)
  let prop02 aut =
    List.iter (
        fun state ->
        let bs= (* the B such that delta(q,(0,b)) is in the same scc as q*)
          List.filter
            (fun b ->
             let letter = Letter.ParenG b in
             let suc = successor state letter in 
             state.scc == suc.scc
            ) (Subset.enumerate aut.dimension)
        in 
        match bs with
        |[] ->
          raise (NotReg.T (NotReg.Prop02exists state))
        | b:: bs ->
           let b_inter=(* the intersection of every element of bs *)
             List.fold_left Subset.inter b bs in
           (* we check if the intersection lead to the same scc *)
           let letter = Letter.ParenG b_inter in
           let suc = successor state letter in 
           if suc.scc != state.scc then 
             raise (NotReg.T (NotReg.Prop02array (state)));
           state.oop.Oop.b <- b_inter;
           Subset.iter 
             (fun b ->
              Subset.iter 
                (fun u ->
                 Oop.lower state.oop u b Oop.Les
                ) (Subset.comp b_inter)
             )b_inter;
           (* if the b class is empty, it goes into b_less *)
           if Subset.is_empty b_inter 
           then aut.b_less <- state :: aut.b_less
      ) aut.cycles;
    (*Finally, we must check that for each element of an scc, we have
            the same B.  *)
    Array.iter (
        fun scc ->
        let state2 = List.hd scc.statesS and
            t = List.tl scc.statesS in
        let b = state2.oop.Oop.b in
        if Subset.is_empty b 
        then aut.b_less_scc <- scc :: aut.b_less_scc;
        List.iter (
            function state  ->
                     if  b<> state.oop.Oop.b  then
                       raise (NotReg.T (NotReg.Prop02state (state2, state)))
          ) t;
        (* Furthemore if there is a non empty b, we put +1 and = at
            true *)
        if not (Subset.is_empty b) then
          (aut.use_equal <- true;
           aut.use_suc <- true;
           aut.use_zero <- true )
      ) aut.sccs

               
  (** Check property3 after the 2. Compute the Us and the Z *)
  let prop03 aut =
    List.iter (* We must do the same verification on each state *)
      (fun state ->
       let b = state.oop.Oop.b in
       let ucs = Subset.enumerate_sub (Subset.comp b) in(* the sets disjoint from Bc *)
       let ucs = (* such that delta(state,(U,Bc)) is in the same scc as state*) 
         List.filter (
             fun u ->
             let letter = Letter.ParenT (u, b) in
             let suc = successor state letter in 
             state.scc == suc.scc 
           ) ucs in
       let cut = Subsets.cut ucs aut.dimension in (*the subsets Uc *)
       (*we check if every element lead to the scc of state.  We
       could also check if cut is a subet of ucs*)
       Subsets.iter (
           fun u ->
           let letter = Letter.ParenT (u, b) in
           let suc = successor state letter in 
           if state.scc != suc.scc  then 
             raise (NotReg.T (NotReg.Prop03array (state,u)))
         ) cut;
       state.oop.Oop.u <- cut;
       let z= Subset.comp (Subset.union (Subsets.union cut) b) in
       state.oop.Oop.z <- z;
       Subset.iter 
         (fun elt_z ->
          Subset.iter 
            (fun i ->
             Oop.lower state.oop elt_z i Oop.Les
            ) (Subset.comp z)
         )z;
       if not (Subset.is_empty z)
       then ( aut.use_zero <- true)
      ) aut.cycles ;
    (* Finally, we must check that for each element of an scc, there are
  the same Ucs. Hence they are equal*)
    Array.iter (
        fun scc ->
        let h = List.hd scc.statesS in
        let t = List.tl scc.statesS in
        List.iter (
            function state  ->
                     if state.oop.Oop.z <> h.oop.Oop.z || state.oop.Oop.u <> h.oop.Oop.u  then
                       raise (NotReg.T (NotReg.Prop03state (h, state)))
          ) t
      ) aut.sccs
               
  (** This function put the visited of every state of a scc to false. *)
  let unvisit aut =
    List.iter (fun state -> state.visited <- false) aut.cycles

  (** Assuming: on each cycle, visited is all true or all false.
State.visited is false.
visited become true on the cycle of state.  
inv letter are added as inverse on the cycle.
return true if there a cycle, and the size of the cycle
   *)
  let is_cyclic state letter inv inf sup=
    let rec aux state2 size=
      if state2.visited 
      then (false, -1)
      else (state2.visited <- true;
            let suc = successor state2 letter in
            add_transition suc inv state2 ;
            Subset.iter 
              (fun i ->
               Subset.iter 
                 (fun j ->
                  Oop.lower state2.oop i j Oop.Les
                 ) sup
              ) inf;
            if suc == state 
            then (true, size+1)
            else aux suc (size + 1)
           )
    in aux state 0
           
  (** Check that delta(q, (I,B}) is ({},B} cyclic; add inverse (emptyset,B)^-1 *)
  let prop04 aut =
    let d = aut.dimension in
    List.iter 
      (fun i->
       List.iter 
         (fun k ->
          List.iter 
            (fun state ->
             let letter = Letter.ParenG k
             and inv = Letter.ParenI k in
             let suc = successor state (Letter.ParenT (i,k)) in
             if not suc.visited then
               if not (fst (is_cyclic suc letter inv i k)) then
                 raise (NotReg.T (NotReg.Prop04 (state, i,k)))
            ) aut.cycles;
          unvisit aut
         ) (Subset.enumerate_sub (Subset.comp i))
      ) (Subset.enumerate d)
      
  (** add every successor of the form [U,B] and [U]*)
  let addCrochet aut=
    List.iter (* looping on each U*)
      (fun u  ->
       List.iter (* looping on each B to add [U,B]*)
         (fun b -> 
          List.iter  (* looping on each state with a cycle *)
            (fun state->
             let crochet = Letter.CrochetT (u,b) 
             and paren = Letter.ParenT (u,b) 
             and inv = Letter.ParenI b in
             let succ1 = successor state paren in
             let succ = successor succ1 inv in
             add_transition state crochet succ;
             if Subset.is_empty b then
               let crochet = Letter.CrochetD u in
               add_transition state crochet succ
            ) aut.cycles
         ) (Subset.enumerate_sub (Subset.comp u))
      )(Subset.enumerate aut.dimension)

  (** Check that delta(q, [I,B]) is [I,B] cyclic; add inverse [I,B]^-1 *)
  let prop05_06 aut =
    Array.iter 
      (fun scc ->
       List.iter 
         (fun k'->
          let k = Subset.union k' scc.a_state.oop.Oop.b in
          List.iter 
            (fun i ->
             let letter = Letter.CrochetT (i,k)
             and inv = Letter.CrochetI (i,k) in
             (* fprintf std_formatter "05 on %a, scc=%d@." Letter.printf letter scc.idS; *)
             List.iter 
               (fun state ->
                let suc = successor state letter in
                if not suc.visited then(
                  (* fprintf std_formatter "working on delta(%a,%a)=%a, adding %a@." state_print_name state Letter.printf letter state_print_name suc Letter.printf inv; *)
                  let cyclic, size = is_cyclic suc letter inv i k in
                  if not cyclic then
                    raise (NotReg.T (NotReg.Prop05 (state, i, k)))
                  else 
                    try 
                      if size <> Hmap.find i suc.scc.ku
                      then raise (NotReg.T (NotReg.Prop06 (suc, i)))
                    with
                    | Not_found ->
                       (suc.scc.ku <- Hmap.add i size suc.scc.ku;
                        if (Math.gcd size aut.base)>1 
                        then raise (NotReg.T (NotReg.Prop06Prime (state, i,k)))
                       )
                )
               )scc.statesS ;
             unvisit aut
            )(Subset.enumerate_sub (Subset.comp k));
         ) (Subset.enumerate_sub (Subset.comp scc.a_state.oop.Oop.b))
      ) aut.sccs
      
  (** add every successor of the form <U,B> and <U>*)
  let addChevron aut=
    List.iter  (* looping on each state with a cycle *)
      (fun state->
       List.iter (* looping on each B to add <U,B> and <U>*)
         (fun bs -> 
          let b = Subset.union (Subsets.union bs ) state.oop.Oop.b in
          List.iter (* looping on each U*)
            (fun u  ->
             let chevron = Letter.ChevronT (u,b) 
             and crochet = Letter.CrochetT (u,b) 
             and inv = Letter.CrochetI (u,b) in
             let succ = successor state crochet in
             let succ2 = successor succ inv in
             add_transition state chevron succ2;
             if Subset.is_empty b 
             then let chevron = Letter.ChevronD u
                  in add_transition state chevron succ2
            ) (Subset.enumerate_sub (Subset.comp b)) ;
         )(Subsets.enumerate state.oop.Oop.u)
      ) aut.cycles
              


  (** this function put = and < to true in the automaton if
    necessary *)
  let checkPredicates aut =
    List.iter
      (fun state->
       for i=0 to aut.dimension -1 do
         for j=0 to aut.dimension -1 do
           match state.oop.Oop.comparison.(i).(j) with
           |Oop.Eq ->
             aut.use_equal <- true
           |Oop.Les |Oop.Gre ->
                      aut.use_equal <- true;
                      aut.use_less <- true
           |Oop.Bot -> ()
         done 
       done
      ) aut.cycles
      
  (** For each subsets us, the subset of the union of u divides the
  lcm of the subset of its element *)
  let prop07 aut =
    Array.iter
      (fun scc ->
       List.iter 
         (fun subsets ->
          let lcm = ref 1 in
          Subsets.iter 
            (fun subset->
             let k_subset = Hmap.find subset scc.ku in
             lcm := Math.lcm !lcm k_subset;
             scc.k <- Math.lcm scc.k k_subset;
             if scc.k >1 then aut.use_mod <- true
            ) subsets;
          let subset = Subsets.union subsets in
          try
            let k_subset= Hmap.find subset scc.ku in
            if !lcm mod k_subset >0 
            then raise (NotReg.T (NotReg.Prop07 (scc, subset)))
          with
          |Not_found ->
            (* in this case, an union of u is not an u *)
            raise (NotReg.T(NotReg.Prop03union (scc, subset)))
         ) (Subsets.enumerate scc.a_state.oop.Oop.u)
      ) aut.sccs

      
  let prop08_09 aut =
    let d = aut.dimension in
    let b = aut.base in
    let abd = aut.alphabet in
    List.iter
      (fun state->
       List.iter 
         (fun letter ->
          let is08 = ref false 
          and j = ref ( -1)
          and j_val = ref b 
          and cqs = ref (Subset.empty d) (* the set of position to decrement *)
          and jqs = ref (Subset.empty d)(* the set of position remaining in b*)
          and left = successor state (Letter.Letter letter) in
          for i=0 to d -1 do
            let li = IntAr.get letter i in
            if li = b-1 && Subset.ins state.oop.Oop.b i
            then 
              jqs := Subset.add !jqs i;
            if (li>0 && Subset.ins state.oop.Oop.z i) ||
                 (li<b-1 && Subset.ins state.oop.Oop.b i)
            then is08 := true;
            (* we search for the minimal strictly positive element,
            which does not stay in the b class *)
            if (li>0 && (* the letter must be positive *)
                  (li < !j_val || (* it must be less *)
                     (li = !j_val && (*for lexicographical order *)
                        state.oop.Oop.comparison.(i).(!j) = Oop.Les))
                  && (li<b-1 || (*and if b-1 it must be out of the b-class*)
                        not (Subset.ins state.oop.Oop.b i)))
            then (j_val := li;
                  j := i)
          done;
          let is08 = !is08 and j_val = !j_val in
          let is09 = not is08 && j_val <b in
          let i_vqs = ref(Subset.empty d) (* the set I_qs if 08 or V_qs if 09*)
          in 
          for i=0 to d-1 do
            let li = IntAr.get letter i in
            if (is08 && li>0 && Subset.ins state.oop.Oop.z i)|| 
                 (is09  && li= j_val && state.oop.Oop.comparison.(i).(!j)= Oop.Eq)
            then (i_vqs := Subset.add !i_vqs i;
                  cqs:= Subset.add !cqs i)
          done; 
          let cqs = IntAr.decr letter !cqs in
          let right = successor state (Letter.CrochetT (!i_vqs, !jqs)) in
          let right = successor right (Letter.Letter cqs) in
          if (is08|| is09) && left != right then
            raise 
              (if is08 
               then (NotReg.T (NotReg.Prop08 (state, letter, left, right)))
               else (NotReg.T (NotReg.Prop09 (state,letter, left, right))))
         ) abd
      ) aut.cycles
      
      
  (** Test if for each kind of letter of Property 11, the successor is correct *)
  let prop11 aut =
    let b = aut.base in
    (* test q l raise an error if delta(q,l).op <= q.op . l   *)
    let test state letter =
      let suc = successor state letter in
      let concat = Oop.concat state.oop b letter in
      if not (Oop.less_eq suc.oop concat)
      then raise (NotReg.T (NotReg.Prop11 (state, letter)))
    in
    List.iter 
      (fun state ->
       test state (Letter.ParenG state.oop.Oop.b);
       List.iter 
         (fun s ->
          List.iter 
            (fun t ->
             test state (Letter.CrochetT (s,t))
            ) (Subset.enumerate_sub state.oop.Oop.b)
         ) (Subset.enumerate_sub state.oop.Oop.z);
       List.iter 
         (fun s ->
          test state (Letter.CrochetT (s, state.oop.Oop.b))
         ) (Subset.enumerate_sub(Subsets.union state.oop.Oop.u))
      ) aut.cycles
      
  (** Properties 12, 13, 14, 15, 16, 19, 20, 21, 22 *)
  let propsEq aut =
    let dim = aut.dimension  in
    let zero = Letter.zero aut.base dim in
    List.iter
      (fun state->
       (* After Prop 15, we assume that the state has an empty b
     class, so this variable tel us if we need to consider those
     values. It is equivalent to the existence of state in
     aut.b_less *)
       let after15 = Subset.is_empty state.oop.Oop.b 
       in
       (* prop 13 *)
       (let suc = successor state (Letter.CrochetT (Subset.empty dim,state.oop.Oop.b))
        in
        if state != suc
        then raise (NotReg.T (NotReg.Prop13 state)));
       
       List.iter
         (fun i->
          (*prop 16 *)
          if after15
          then(
            let left = successor state (Letter.CrochetD i) in
            let center = successor left (Letter.ChevronD i) in
            let right = successor (successor state (Letter.ChevronD i))
                                  (Letter.CrochetD i)
            in 
            if (left != center || center != right)
            then raise (NotReg.T (NotReg.Prop16 (state,i))));
          
          (*prop19*)
          if after15
          then(let left = successor (successor state zero)
                                    (Letter.CrochetD i) in
               let right = ref state 
               in 
               for j = 1 to aut.base do
                 right := successor !right (Letter.CrochetD i)
               done;
               let right = successor !right zero
               in 
               if left!=right 
               then raise (NotReg.T (NotReg.Prop19 (state,i)))
              );
          
          (* prop 20 *)
          if after15
          then (let left = successor (successor state zero)
                                     (Letter.ChevronD i) in
                let right = successor (successor state (Letter.ChevronD i))
                                      zero
                in 
                if left!=right
                then raise (NotReg.T (NotReg.Prop20 (state,i)))
               );
          
          (* we are going to compute ds many time, we could save time
        here *)
          let ds = Subset.enumerate_sub (Subset.comp i) in
          List.iter
            (fun d ->
             (* testing Prop12 *)
             (let left = successor 
                           (successor state (Letter.ParenG d))
                           (Letter.CrochetD i) in
              let right = ref( successor state  (Letter.CrochetD i)) in
              let letter = Letter.CrochetD (Subset.union d i) in
              for i = 1 to aut.base -1 do
                right := successor !right letter
              done;
              let right = successor !right zero in
              if left!= right 
              then raise (NotReg.T (NotReg.Prop12 (state, i,d))));
             
             
             (* the set of possible value for k*)
             let ks = Subset.enumerate_sub 
                        (Subset.comp 
                           (Subset.union d state.oop.Oop.z)) in
             List.iter 
               (fun k->
                let di = Subset.union d i  in
                (* Prop 14 *)
                (let left = successor (successor state (Letter.CrochetT (k, d)))
                                      (Letter.CrochetD i) in
                 let right = successor (successor (successor state
                                                             (Letter.CrochetD i))
                                                  (Letter.CrochetD k))
                                       (Letter.ChevronD di)
                 in
                 if left != right 
                 then raise (NotReg.T (NotReg.Prop14 (state, d, i, k))));
                
                
                (* prop 21 *)
                if after15
                then (let left  = successor (successor state (Letter.ChevronD i))
                                            (Letter.CrochetD k) in
                      let right  = successor (successor (successor state (Letter.CrochetD k))
                                                        (Letter.ChevronD i))
                                             (Letter.ChevronD k)
                      in
                      if left!= right  
                      then raise (NotReg.T (NotReg.Prop21 (state, i,k)))
                     );
                
                (* prop 22 *)
                if after15
                then (let left  = successor (successor state (Letter.CrochetD k))
                                            (Letter.ChevronD i) in
                      let right  = successor (successor (successor state (Letter.ChevronD i))
                                                        (Letter.CrochetD k))
                                             (Letter.ChevronD i)
                      in 
                      if left!= right  
                      then raise (NotReg.T (NotReg.Prop22 (state, i,k)))
                                 
                     );
               )ks
            )ds
         ) (Subset.enumerate dim)
      )aut.cycles
      


  (** This comput the eqivalence class according to ~, assuming <.> are already computed. Compute iota *)
  let comput_eq_18 aut=
    let idE = ref 0 in 
    List.iter
      (fun state ->
       let scc = state.scc in
       if state.eq == eq_dummy 
       then 
         (let eq = {idE = !idE; statesE = State_oop_set.empty ; iota = Array.make_matrix aut.dimension aut.dimension Important} in
          incr idE;
          scc.eqs <- eq :: scc.eqs;
          (* if List.mem_assoc scc eq.sccE  *)
          (* then eq.sccE <- (scc, state) :: eq.sccE *)
          (* we use a recursive function to do a depths first
search over the states.  We do it while state.eq seen are eq_dummy.
if its eq then we stop.  If its something else, its not reg, we return
false and then raise an error, even if its not in the paper, because
it would be harder to deal with it *)
          let rec aux state2 =
            if state2.eq == eq_dummy
            then
              (state2.eq <- eq;
               for i = 0 to aut.dimension -1 do
                 for j = 0 to aut.dimension -1 do
                   if state2.oop.Oop.comparison.(i).(j)= Oop.Bot
                   then eq.iota.(i).(j) <- Ignore
                 done
               done;
               (* if there is already a state with the same oop*)
               if State_oop_set.mem  state2 eq.statesE
               then raise (NotReg.T (NotReg.Prop18 (state2, State_oop_set.find state2 eq.statesE)))
               else
                 eq.statesE <- State_oop_set.add state2 eq.statesE;
               Subsets.iter
                 (fun u ->
                  let suc = successor state2 (Letter.ChevronD u) in
                  (* if not(Oop.bot_eq state.oop suc.oop )  *)
                  (* then raise (NotReg.T (NotReg.Prop18 (state,suc))); *)
                  let ok = aux suc
                  in if not ok 
                     then raise (NotReg.T (NotReg.Prop18bis (state2, u)))
                 )scc.a_state.oop.Oop.u;
               true 
              )
            else state2.eq == eq
          in 
          ignore (aux state)
         (* we can ignore the value returned, there are no possible problem on
 the first state *)
         )
      ) aut.b_less
      
  let prop17 aut =
    let d = aut.dimension in
    List.iter 
      (fun state->
       for i = 0 to d-1 do
         for j = 0 to d-1 do
           if state.eq.iota.(i).(j)= Ignore  && state.oop.Oop.comparison.(i).(j)=Oop.Les
           then (let inter = ref false in
                 for k = 0 to d-1 do
                   if state.oop.Oop.comparison.(i).(k) = Oop.Les &&
                        state.oop.Oop.comparison.(k).(j) = Oop.Les
                   then inter := true
                 done;
                 if not !inter then
                   raise (NotReg.T (NotReg.Prop17 (state,i,j)))
                )
         done
       done
      ) aut.b_less
    
  (** takes a set of state and return their number of distincts oop *)
  let nb_distinct l=
    (* the list of distinct oop *)
    let l = List.fold_left (fun acc s-> Oop.S.add s.oop acc)  Oop.S.empty l
    in Oop.S.cardinal l

  let comp_rel aut = 
    try 
      let d = aut.dimension in
      let abd = aut.alphabet in
      (* a function to compute D^{i,~}*)
      let comp_d di comp value letter=
        let di' = ref(Subset.empty d) in
        for i =0 to d-1 do
          let li = IntAr.get letter i in
          if comp li value || (li=value && Subset.ins di i) 
          then di' := Subset.add !di' i
        done ;
        !di'
      in
      (* an auxiliary function for comp_rel. *)
      let rec add_rel ((i, ((d1l, d2l), (d1g, d2g))) as input) q1 q2=
        let ar = 
          try
            U5M.find input q1.rel 
          with |Not_found ->
                 let ar = Array.make aut.size false in
                 q1.rel <- U5M.add input ar q1.rel;
                 ar
        in
        if not ar.(q2.id) then
          (ar.(q2.id)<- true;
           if q1.cyclic && q2.cyclic 
           then (let usable = ref true in (*true if every position in i are in a u-class *)
                 Subset.iter (fun i -> usable:= !usable && Oop.is_u q1.oop i && Oop.is_u q2.oop i)i;
                 if !usable &&  d1l=d2l && d1g=d2g && (Math.xor q1.final q2.final) 
                 then (aut.use_mod <- true;
                       raise Math.Found));
           List.iter 
             (fun s1 ->
              let ok = ref true in
              let pos = Subset.elt i in
              Subset.iter (fun p-> if IntAr.get s1 p<> IntAr.get s1 pos
                                   then ok := false )i ;
              if !ok then
                for j = 0 to aut.base-1 do 
                  let s2 = Subset.fold (fun s2 k -> IntAr.set s2 k j) s1 i in
                  let s1p = IntAr.get s1 pos 
                  and s2p = IntAr.get s2 pos in
                  let d1l = comp_d d1l (<) s1p s1 in
                  let d2l = comp_d d1l (<) s2p s2 in
                  let d1g = comp_d d1g (>) s1p s1 in
                  let d2g = comp_d d1g (>) s2p s2 in

                  let q1 = successor q1 (Letter.Letter s1)
                  and q2 = successor q2 (Letter.Letter s2)
                  in add_rel (i,(( d1l, d2l),( d1g, d2g))) q1 q2
                             
                done
             ) abd
          )
      in
      let empty= Subset.empty d in
      List.iter 
        (fun i ->
         if not (Subset.is_empty i) 
         then Array.iter 
                (fun q -> add_rel (i, ((empty, empty), (empty, empty))) q q
                ) aut.states
        )(Subset.enumerate d)
    with | Math.Found -> ()
      
  let first = ref true       
  let exists s aut = 
    try 
      check0 aut; 
      print_debug "0 checked !";
      tarjan aut; (*scc ready*)
      print_debug "tarjan done !";
      prop01 aut; 
      print_debug "Prop01 done !";
      addParen aut; (* () added *)
      print_debug "addParen done, () added !";
      prop02 aut;(*B added, suc correct *)
      print_debug "prop2 done, B added, suc correct !";
      prop03 aut;(*Z and U added *)
      print_debug "prop3 done, Z and U added !";
      prop04 aut; (* ()^-1 added *)
      print_debug "prop4 done,  ()^-1 added !";
      addCrochet aut ; (*[] added *)
      print_debug "addCrochet done, [] added !";
      prop05_06 aut; (* Prop05 checked *)
      print_debug "Prop05 checked ";
      addChevron aut; (* <> added *) 
      print_debug "addChevron done,  <> added !";
      checkPredicates aut; (* equal, less correct*)
      print_debug "checkPredicates done,  aut.equal, aut.less correc !";
      prop07 aut; (* Prop09 checked *)
      print_debug "Prop07 checked ";
      prop08_09 aut;
      print_debug "Prop08_09 done !";
      prop11 aut ; 
      print_debug "Prop11 done !";
      propsEq aut ; 
      print_debug "propsEq done !";
      comput_eq_18 aut; (* eq and eqs are correct *)
      print_debug "comput_eq_18 done,  eq and eqs are correct !";
      prop17 aut;
      print_debug "prop17 done !";
      print_debug "\nThe automaton accept a regular set";
      if s= Mod then aut.use_mod <- true;
      if not aut.use_mod then
        (comp_rel aut;
         print_debug "\nWe now know if there is a mod");
      None
    with
    | NotReg.T n -> Some n
    | e->
       if !first then (first := false; raise (E (e, aut))) else raise e
end
  
let exists s aut = Exists.exists s aut
                  
let to_simple aut =
 aut.simple

module rec Construct :
sig 
    val construct :Formula.quantifier -> automaton ->  Formula.t option
end =
struct
  (** compute the correct value of state.step.(i) *)
  let step aut = 
    let rec aux state i =
      if not(Int.S.mem i state.step) then
        (state.step <- Int.S.add i state.step;
         if not state.cyclic then
           Transition.iter
             (fun _ state ->
              aux state (i+1))
             state.delta
        )
    in aux aut.states.(0) 0
  
  (** compute the kappa value between states in a scc*)
  let kappa_scc aut =
    List.iter 
      (fun from ->
       (* let equiv_class = Subsets.add from.oop.Oop.z (Subset. add from.oop.Oop.b  *)
       (** put array ass path (from -> to) and resume the path *)
       let rec aux onto array =
         from.kappa <- SM.add onto array from.kappa ;
         Subsets.iter 
           (fun u ->
            let letter = Letter.CrochetD u in
            let suc = successor onto letter in
            if not (SM.mem suc from.kappa) then
              (let array = Array.copy array in
               Subset.iter 
                 (fun i -> 
                  array.(i) <- array.(i) + 1) u;
               aux suc array
              )
           ) from.oop.Oop.u
       in aux from (Array.make aut.dimension 0)
      ) aut.b_less

  (** The rules to rewrite element in an scc *)
  let rewrite aut =
    List.iter
      (fun scc ->
       let init_state = List.hd scc.statesS 
       in
       Subsets.iter 
         (fun u ->
          let letter = Letter.CrochetD u
          and s = Subset.elt u
          and i = ref 0
          and state = ref init_state
          in 
          while 
            (** while the element at pos s equals i*)
            (SM.find !state init_state.kappa ).(s) = !i
          do 
            incr i;
            state := successor !state letter 
          done;
          scc.rewriting <- Hmap.add u (!i, SM.find !state init_state.kappa ) scc.rewriting
         ) scc.a_state.oop.Oop.u
      ) aut.b_less_scc

  (** compute the element which are accessible from a state by [I]<>*
  where I is a subset of the zero class *)
  let zero aut =
    List.iter 
      (fun from->
       let rec aux onto =
         if not( CM.mem onto.scc from.zeros)  then
           (from.zeros <- CM.add onto.scc onto from.zeros;
            List.iter
              (fun sub ->
               aux (successor onto (Letter.ChevronD sub))
              ) (Subset.enumerate aut.dimension)
           )
       in
       List.iter 
         (fun i ->
          let inter = successor from (Letter.CrochetD i) in
          aux inter
         )(Subset.enumerate_sub from.oop.Oop.z)
      ) aut.cycles

  (** compute existential state.formula for state with a cycle and with empty b class *)
  let formula_cycle aut =
    let x = aut.var in 
    List.iter 
      (fun state0->
       let z0 = state0.oop.Oop.z in
       let formula = 
         List.fold_left 
           (* we work on each scc, from the leaves upto scc0 *)
           (fun formula scc ->
            if not(CM.mem scc state0.zeros ) 
            then
              formula
            (*we only work on a scc if it is accessible from q_0 and has an empty b class*)
            else 
              (* the state in scc accessible from state0 by [I] <>* *)
              let zero = CM.find scc state0.zeros  in
              (* the formula for the scc *)
              let formula_scc=  Oop.formula_eq scc.a_state.oop z0 x in
              (* the formula for each accepting state *)
              let state_formula = 
                List.fold_left
                  (fun acc state ->
                   if state.final then
                     (let conj = ref [] in
                      (* the < part *)
                      Subsets.iter
                        (fun us ->
                         let u = Subset.elt us in
                         Subsets.iter 
                           (fun vs ->
                            let v = Subset.elt vs in
                            if state.oop.Oop.comparison.(u).(v)= Oop.Les
                            then 
                              let new_oop = Oop.unite state.oop  us vs in
                              let word = Oop.gen_ex new_oop z0 aut.base in
                              let onto = successors state0 word in
                              if onto.oop.Oop.comparison.(u).(v)= Oop.Les
                              then conj := Formula.lesseq (x.(u), x.(v)) :: !conj
                              else conj := (Formula.lessst (x.(u), x.(v))) :: !conj
                           ) scc.a_state.oop.Oop.u 
                        ) scc.a_state.oop.Oop.u ;
                      
                      
                      (*the mod part *)
                      let init_var = ref x                    
                      in
                      
                      let i = ref 0 in (*a counter to have a unique letter for the variable *)
                      Subsets.iter 
                        (fun u ->
                         let s = Subset.elt u in
                         (*if we have "m" in pos u times we rewrite it as mu*)
                         let (m,mu) = Hmap.find u scc.rewriting  in
                         let next_var = Formula.newBoundArray aut.dimension  ("u^"^(string_of_int !i)^"_") in
                         let k_u = Hmap.find u scc.ku in (* the value of the modulo used for element of u *)
                         let known = ref [] in 
                         for a=0 to k_u -1 do
                           let a_over_m = a / m in (* the number of time to apply rewriting rule *)
                           let left = Formula.mod_ (!init_var.(s), a, k_u) in
                           known := left :: !known ;
                           let right = ref [] in
                           (*we apply every rewritting rules *)
                           Subsets.iter 
                             (fun u2->
                              (Subset.iter 
                                 (fun s2 ->
                                  let equiv =
                                    if u2 <> u 
                                    then (* we apply rewriting rule for element different from u *)
                                      Formula.add ((!init_var).(s2), (a_over_m * mu.(s2)), next_var.(s2)) 
                                    else Formula.add (next_var.(s2), (a_over_m * k_u), (!init_var).(s2))
                                  in
                                  right := equiv :: !right
                                 ) u2
                              )
                             ) scc.a_state.oop.Oop.u;
                           let right = Formula.and_list !right in 
                           conj := (Formula.imply (left, right)) :: !conj
                         done;
                         let ass = Formula.assert_ (Formula.or_list !known) in
                         (* fprintf std_formatter "we assert %a@." Formula.printf ass; *)
                         conj := ass :: !conj;
                         init_var := next_var;
                         incr i
                        ) scc.a_state.oop.Oop.u;
                      (* now, init_var correspond to the minimal int array equivalent to x *)
                      let kappa = SM.find state zero.kappa in
                      Subsets.iter 
                        (fun u->
                         let s = Subset.elt u in
                         let mod_f = Formula.mod_ (!init_var.(s), kappa.(s) , Hmap.find u scc.ku) in
                         conj := (mod_f) :: !conj 
                        ) scc.a_state.oop.Oop.u;
                      let conj = Formula.and_list !conj in
                      (* fprintf std_formatter "The conj is %a@." Formula.printf conj ;  *)
                      conj :: acc
                     ) 
                   else acc
                  ) [] scc.statesS
              in 
              let if_scc = Formula.or_list state_formula in
              let if_scc = Formula.imply (formula_scc, if_scc)
              and if_not_scc = Formula.imply ((Formula.not_ formula_scc), formula)
              in Formula.and_list [ if_scc; if_not_scc]
           ) Formula.false_ aut.b_less_scc
       in
       (* fprintf std_formatter "generating formula %a of state %d@." Formula.printf formula state0.id; *)
       state0.formula <- Some (formula)
      ) aut.cycles

  (** generate the formula from initial state assuming every
      precedent function have been applied *)
  let construct_exists aut =
    let x = aut.var in
    let d = aut.dimension in
    let b = aut.base in
    (* s.(i).(j) is s^{q_i,j} and t.(i).(j) is t^i_j as in the paper *)
    let s = 
      Array.init aut.size
                 (fun i -> 
                  Array.init aut.size 
                             (fun j -> 
                              let descr = "s^{q_"^(string_of_int i)^","^ (string_of_int j)^"}" in
                              Formula.newBoundVar descr)) in
    let conj = ref [ (Formula.bool (s.(0).(0)))] in 
    (* the first element of t is the number of step, the other is the dimension *)
    let t =
      Array.init aut.size 
                 (fun i ->
                  let descr = "t^"^(string_of_int i)^"_" in
                  Formula.newBoundArray d descr
                 ) in
    t.(0) <- x;
    let bt= ref 1 in
    Array.iter 
      (fun t ->
       for k = 0 to d -1 do
         let known =ref [] in
         for j = 0 to b-1 do
           known := Formula.mod_ (t.(k), !bt*j, !bt*b)  :: !known
         done;
         conj := Formula.mod_ (t.(k), 0, !bt) :: !conj;
         conj := Formula.assert_ (Formula.or_list !known) :: !conj;
       done;
       bt := !bt * b
      ) t;
    (* the set of fact *)
    Array.iter 
      (fun state ->
       let bm= ref 1 in (* b^m *)
       for m = 0 to aut.size-1 do (* the number of step*)
         if Int.S.mem m state.step then
           (* we generate the code for state with a cycle *)
           (let right = 
              if state.cyclic then
                (let formula = Formula.change_free t.(m) (Math.sure state.formula) in
                 Formula.power !bm formula)
              (*state without cycle, we must not do the last value, else we
           have an index out of bound in in*)
              else
                (if m < aut.size -1 
                 then
                   (let conj' = ref [] in
                    List.iter 
                      (fun a ->
                       let letter = Letter.Letter a 
                       in 
                       let left = ref []
                       and right = ref [ (Formula.bool (s.((successor state letter).id).(m+1)))]
                       in 
                       for i = 0 to aut.dimension -1 do 
                         let ai = IntAr.get a i in
                         let l = (Formula.mod_ (t.(m).(i) , ai * !bm, b* !bm) ) in
                         let r = (Formula.add (t.(m+1).(i), ai * !bm,t.(m).(i))) in
                         left := l :: !left; right := r :: !right
                       done ;
                       let left = Formula.and_list !left and right = Formula.and_list !right in
                       let f = Formula.imply (left, right) in
                       conj' := f :: !conj'
                      ) aut.alphabet;
                    Formula.and_list !conj'
                   )
                 else Formula.false_ (*This can not happen *)
                ) in
            let left = Formula.bool (s.(state.id).(m)) in
            let f = Formula.imply (left, right) in
            conj := f :: !conj
           );
         bm := !bm * b
       done 
      ) aut.states;
    Formula.and_list !conj

  (** construct if mod is not needed *)
  let rec construct_les quant simple =
    let d =simple.Simple.dimension in
    let vars = Formula.free d in
    if d=0 
    then if simple.Simple.states.(0).Simple.final 
         then Formula.true_
         else Formula.false_
    else
      let zeros = Simple.remove_0 simple in
      let init_cylic = ref false in
      Array.iter 
        (fun state->
         IntAr.M.iter 
           (fun _ suc-> 
            if suc =0 then init_cylic := true
           ) state.Simple.delta
        ) simple.Simple.states;
      let f = 
        if !init_cylic
        then 
          ((*in theory, elim change nothing as the formula shoold already
        be quantifier free*)
            let aut = of_simple simple in
            ignore (Exists.exists Less aut);
            aut.use_mod <- true;
            Math.sure (Construct.construct quant aut )
          (* I use Construct. because the module is recursive, hence I
          can use a function not yet defined *)
          )
        else
          let simple' = Simple.remove_1 simple in
          let right1 = construct_les quant simple' in
          let right1 = Formula.add1 right1 in
          let left1 = List.map
                        (fun i ->
                         Formula.not_ (Formula.const (vars.(i),0))
                        )(Math.gen_list 0 (d-1)) in
          let left1 = Formula.or_list left1 in
          let f1 = Formula.imply (left1, right1) in (* the formula if no element is 0 *)
          let f0 = Array.mapi 
                     (fun i simplei->
                      let left = Formula.const (vars.(i),0 ) in
                      let right = construct_les quant zeros.(i) in
                      let right = Formula.move i right in
                      Formula.imply(left,right)
                     ) zeros in
          let f0 = Array.to_list f0 in
          let f0 = Formula.and_list f0 in
          Formula.and_list [f0; f1] 
      in
      (* fprintf std_formatter "%a @.gives %a@.@."Simple.printf simple Formula.printf f; *)
      f


  let construct_qf aut =
    let rec aux state = 
      match state.formula with
      | None -> 
         let t = Transition.map aux state.delta in
         let f = Formula.trans aut.base t in
         state.formula <- Some f;
         f
      | Some f -> f
    in aux aut.states.(0)

  let construct quant aut =
    (* fprintf std_formatter "Constructing %a" aut_printf aut; *)
    if aut.use_mod 
    then
      (step aut;
       print_debug "step computed";
       kappa_scc aut;
       print_debug "kappa between states of a scc computed";
       rewrite aut;
       print_debug "the rewritting rule for each scc and each U_i are computed";
       zero aut;
       print_debug "The link from each state to the equivalent state in each accessible scc is computed";
       match quant with 
       | Formula.Free ->
          formula_cycle aut;
          print_debug "the base formula from each state with a cycle is computed\n";
          Some(construct_qf aut)
       | Formula.Existential ->
          formula_cycle  aut;
          print_debug "the base formula from each state with a cycle is computed\n";
          Some( Formula.elim Math.N (construct_exists aut))
       |Formula.No_formula ->
         None
      )
    else
      let simple = to_simple aut in
      Some (construct_les quant simple)
end

let construct = Construct.construct

type return = 
  | Formula of Formula.t option
  | Error of (automaton* NotReg.t)

let construct_simple typ quant simple = 
  let aut = of_simple  simple in
  match exists typ aut with 
  | None -> Formula (construct quant aut)
  | Some n -> Error (aut,n)

(** Check property 10 *)
let checkN neg aut = 
  List.iter
    (fun state ->
     if not (Oop.check state.oop neg)
     then raise (N (NotReg.Prop10 (state,neg), aut))
    ) aut.cycles

