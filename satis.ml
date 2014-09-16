open Hashcons
open Format

type t_node = Zero 
       | Suc  of term
       | F  of term
       | Var of int
and  term = t_node Hashcons.hash_consed 

type comp = Eq | Less | Diff

let rec printf ppf t = match t.node with
  | Zero -> fprintf ppf "0"
  | F t -> fprintf ppf "f(%a)" printf t
  | Suc t -> fprintf ppf "%a+1" printf t
  | Var t -> fprintf ppf "x%d"  t

type atom = (term * comp* term)

module T_node = 
  struct 
    type t = t_node
             
    let  equal t u = 
      match (t, u) with 
      | Suc t , Suc u ->  t== u
      | Zero, Zero -> true
      | Var t , Var u ->  t=u
      | F t, F u ->  t== u
      | _, _ -> false

    let hash f = abs (match f with
      | Suc t -> 2* t.hkey +1
      | F t  -> 3* t.hkey +2
      | Zero -> 0
      | Var x -> x)
end
module T_node_ = 
  struct 
    type t= term
             
    let  equal = ( == )

    let hash f = f.hkey
end

module Tterm = Hashcons.Make(T_node)
let tt = Tterm.create 251

module Hterm = Hashtbl.Make (T_node_)
exception Contradiction


let tc f = 
  Tterm.hashcons tt f

type equiv_proof =
  | Assertion_
  | Identity 
  | Transitivity of term
  | Induction of (term * term)
  | Succ_ of (term * term)

type gre_proof =
  | Assertion
  | Fact
  | Eq_left of term
  | Eq_right of term
  | Middle of term
  | Succ of (term * term)
  | Pred of (term * term)


let satis ppf l=
  try 
    let z = tc Zero in
    let terms = Hterm.create 251 in
    let l = (z, Eq, z)::l in (*because it's easyer with 0 *)
    let value = Hterm.create 251 in

    let rec init_map (equiv, greater, lesser) f = 
      if not (Hterm.mem terms f) then
        (Hterm.add terms f ()  ;
         let singleton = Hterm.create 251 in
         Hterm.add singleton f Identity;
         let equiv = Hmap.add f singleton equiv and
             greater = Hmap.add f (Hterm.create 251) greater and
             lesser = Hmap.add f (Hterm.create 251) lesser  in
         match f.node with 
         | Suc t | F t -> init_map (equiv, greater, lesser) t 
         | _ -> (equiv, greater, lesser))
      else  (equiv, greater, lesser)
    in 
    let (equiv: (t_node,equiv_proof Hterm.t) Hmap.t), greater, lesser =
      List.fold_left
        (fun (equiv, greater, lesser) (u,_,v) ->
         let ( equiv, greater, lesser)= init_map (equiv, greater, lesser) u in
         init_map (equiv, greater, lesser) v
        )(Hmap.empty, Hmap.empty, Hmap.empty) l

    in 
    let rec add_equiv u v by = 
      if Hterm.mem terms u && Hterm.mem terms v && not (Hterm.mem (Hmap.find v equiv) u)
      then (
        Hterm.add  (Hmap.find v equiv) u by;
        Hterm.add (Hmap.find u equiv)  v by ;
        add_equiv (tc (F u)) (tc (F v)) (Induction (u, v));
        add_equiv (tc (Suc u)) (tc (Suc v)) (Induction (u, v));
        let propage u v =
          Hterm.iter (fun w _ -> add_equiv  u w (Transitivity v)) (Hmap.find v equiv)
        in 
        propage u v ; 
        propage v u ;
        match u.node, v.node with 
        | Suc u' , Suc v' -> add_equiv u' v' (Succ_ (u,v))
        | _, _ -> ()
      )
    in
    List.iter (function 
                | (u, Eq, v) -> add_equiv u v Assertion_
                | _ -> ()) l;
    
    let rec proof_equals u v n =
      for i= 0 to n-1 do 
        fprintf ppf "-"
      done ; 
      fprintf ppf "%a = %a because " printf u printf v;
      let by = Hterm.find (Hmap.find v equiv) u in
      match by with 
      | Assertion_ -> 
         fprintf ppf "it is an atom@."
      | Identity -> 
         fprintf ppf "they are the same symbol.@."
      | Transitivity w -> 
         fprintf ppf "they are both equals to %a.@." printf w;
         proof_equals u w (n+1);
         for i= 0 to n-1 do 
           fprintf ppf "-"
         done ; 
         fprintf ppf "-@.";
         proof_equals w v (n+1)
      | Induction (u, v)-> 
         fprintf ppf "by induction.@.";
         proof_equals u v n
      | Succ_  (u, v)-> 
         fprintf ppf "their successors are equal.@.";
         proof_equals u v n
    in
    List.iter (
        function 
        |  (u, Diff, v) -> 
            if Hterm.mem (Hmap.find v equiv) u
            then (
              fprintf ppf "%a and %a are assumed to be different but @." printf u printf v;
              proof_equals u v 0_;
              raise Contradiction
            )
        | _ -> ()) l;
    
    let rec proof_inf u v n= 
      for i= 0 to n-1 do 
        fprintf ppf "-"
      done ; 
      fprintf ppf "%a < %a because " printf u printf v;
      let by = Hterm.find (Hmap.find u greater) v in
      match by with 
      | Assertion ->  fprintf ppf "it is an atom.@."
      | Fact ->  fprintf ppf "it is the definition of +1.@."
      | Eq_left  w ->  fprintf ppf "%a = %a < %a.@." printf u printf  w  printf v;
                          proof_equals u w (n+1);
                          for i= 0 to n-1 do 
                            fprintf ppf "-"
                          done ; 
                          fprintf ppf "-@.";
                          proof_inf w v  (n+1)
      | Eq_right  w -> fprintf ppf "%a < %a = %a.@." printf u printf  w  printf v;
                          proof_inf u w  (n+1);
                          for i= 0 to n-1 do 
                            fprintf ppf "-"
                          done ; 
                          fprintf ppf "-@.";
                          proof_equals w v (n+1)
      | Middle  w ->   fprintf ppf "%a < %a < %a.@." printf u printf  w  printf v;
                          proof_inf u w  (n+1);
                          for i= 0 to n-1 do 
                            fprintf ppf "-"
                          done ; 
                          fprintf ppf "-@.";
                          proof_inf w v  (n+1)
      | Succ  (u,v) 
      | Pred  (u,v)  ->   fprintf ppf "%a < %a.@." printf u  printf v;
                          proof_inf u v n
    in

    let rec add_inf u v by = 
      if Hterm.mem terms u && Hterm.mem terms v && not (Hterm.mem (Hmap.find u greater)  v)
      then (
        Hterm.add (Hmap.find u greater)  v by;
        Hterm.add (Hmap.find v lesser) u by  ;
        if v.node = Zero || v==u
        then (proof_inf u v 0;
              raise Contradiction);
        add_inf (tc (Suc u)) (tc (Suc v)) (Pred (u , v));
        Hterm.iter (fun w _ -> add_inf u w (Middle v)) (Hmap.find v greater);
        Hterm.iter (fun w _ -> add_inf u w (Eq_right v)) (Hmap.find v equiv);
        Hterm.iter (fun w _ -> add_inf w v (Middle u)) (Hmap.find u lesser);
        Hterm.iter (fun w _ -> add_inf w v (Eq_left u)) (Hmap.find u equiv);
        
        match u.node, v.node with 
        | Suc u' , Suc v' -> add_inf u' v' (Succ (u,v))
        | _, _ -> ()
      )
    in
    List.iter (function 
                | u, Less, v  -> add_inf u v Assertion
                | _ -> ()) l;
    Hterm.iter (fun k _ -> match k with | ({node=Suc u} as v) -> add_inf u v Fact |_ -> ()) terms; 
    
    let next = ref (Some(tc Zero))
    and l = ref [] and n= ref 0 in
    while Hterm.length terms > 0 do
      let cur = Math.sure !next in 
      next:=  None ;
      let equiv_cur = Hmap.find cur equiv in
      let greater_cur = Hmap.find cur greater in
      l := equiv_cur :: !l;
      Hterm.iter 
        (fun u _ ->
         let s = tc (Suc u) in
         if Hterm.mem terms s 
         then next := Some s;
         Hterm.iter 
           (fun v _ ->
            Hterm.remove (Hmap.find v lesser) u
           ) greater_cur;
         Hterm.remove terms u;
         Hterm.add value u !n
        ) equiv_cur;
      if !next = None 
      then 
        Hterm.iter 
          (fun t _->
           let less_t = Hmap.find t lesser in
           if Hterm.length less_t =0
           then next := Some t
          ) terms;
      incr n
    done;

    let rec print_res=
      function 
      | [] -> 0
      | h :: t -> 
         let n=  print_res t in
         fprintf ppf "%d: " n;
         Hterm.iter (fun i _ ->
                     match i.node with 
                     | F t -> fprintf ppf "f(%d)" (Hterm.find value t)
                     | Var t -> fprintf ppf "X%d" t
                     | _ -> ()
                    ) h;
         fprintf ppf ".@.";
         n+1
    in 
    ignore (print_res !l); 
    true
  with |Contradiction -> false ;;

type t =
  | Fun of t
  | Const of int
  | AddConst of int * t
  | V of int 

type formula = 
  | And of formula list
  | Or of formula list
  | Atom of t * comp * t

let rec simpl_term t=
  tc (match t with
      | Fun f -> F (simpl_term f)
      | Const 0 -> Zero
      | Const i -> Suc (simpl_term  (Const (i-1)))
      | AddConst (0, t) -> (simpl_term t).node
      | AddConst (1, t) -> Suc (simpl_term t) 
      | AddConst (n, t) -> Suc (simpl_term (AddConst (n-1, t)))
      | V i -> Var i
     )


let rec disj = function
    | And [] -> [[]]
    (* | Or [] -> [] *)
    | And [e] -> disj e
    | And (h::t) ->
       (* let l = List.map aux l in *)
       (* let rec a nf= function  *)
       (*   | [] -> nf *)
       (*   | h::t -> *)
       (*      let rec b nf = function  *)
       (*        | [] -> nf *)
       (*        | h::t -> *)
                 
       (*      in *)
       (*      a (b nf h) t *)
       (* in *)
       (* a [[]] l *)
       let l = disj h
       and r = disj (And t) in
       let rec a acc = function
         | [] -> acc
         | h :: t -> a ((List.map (fun t -> h @ t) r)@ acc) t
       in a [] l
       
    | Or l ->
       List.concat (List.map disj l)
    | Atom (t, c, t') ->
       [[(simpl_term  t, c, simpl_term t')]]

let satisfiability ppf f = 
  let l = disj f in
  List.exists (satis  ppf) l;;

  fprintf std_formatter
          (if satisfiability std_formatter (And [Atom (AddConst (2, V 2), Eq, V 0) ; Atom( V 0, Less, V 2)] )
           then "satisiable@.@."
           else "unsatisfiable@.@.")
  ;;

  fprintf std_formatter
          (if satisfiability std_formatter (And [Atom (AddConst (2, V 0), Eq, Fun(V 2)) ; Atom( V 0, Less, Fun(V 2))] )
           then "satisiable@.@."
           else "unsatisfiable@.@.")
  ;;
