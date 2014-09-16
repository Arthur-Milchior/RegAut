open Format
(** Comparaison element between two position. Everything initialised
at equal.  *)
type comp = Les | Eq | Gre | Bot ;;

(** true if c1 is less or equal c2 for the order of oop*)
let less_eq_comp c1 c2 =
  match c1, c2 with 
  |Les, Les | Gre, Gre | _, Eq | Bot ,_ -> true
  | _, _ -> false

(** the greatest element less than c1 and c2 *)
let greatest_comp c1 c2 =
  match (c1,c2) with 
  |Bot, _ | _, Bot | Les, Gre| Gre, Les -> Bot
  |Les, _ | _,Les -> Les
  |Gre, _ | _,Gre -> Gre
  |Eq, Eq -> Eq

(** the opposite of a component *)
let op_comp = function
  | Les -> Gre
  | Gre -> Les
  | a -> a


type t = {size : int ;
          mutable z : Subset.t;
          mutable b: Subset.t;
          mutable u : Subsets.t ;
          comparison: comp array array}
type s = t


let conf_printf ppf c=
  fprintf ppf 
          (match c with 
           | Eq ->  "="
           | Bot ->  "⊥"
           | Les -> "<"
           | Gre -> ">" )

let table_printf ppf t=
  Pretty.print_array 
    ppf "@[[" ",@," "]@]" t.comparison
    (fun ppf _ line ->
     Pretty.print_array 
       ppf "@[[" ",@," "]@]" line
       (fun ppf _ elt ->
        conf_printf ppf elt
       ) 
    ) 

let eq_printf ppf t=
  fprintf ppf "@[(Z=";
  Subset.printf ppf t.z;
  fprintf ppf "@,,U=";
  Subsets.printf ppf t.u;
  fprintf ppf "@,,B=";
  Subset.printf ppf t.b;
  fprintf ppf ")@]"

let printf ppf t=
  fprintf ppf "%a@,,%a"  eq_printf t table_printf t

let clone t =  {t with
                comparison = Array.map (Array.copy) t.comparison
               }

let init d =  { size = d;
                z= Subset.empty d; 
                b= Subset.empty d; 
                u= Subsets.empty d;
                comparison = Array.make_matrix d d Eq}

let less_eq o1 o2 =
  let d = o1.size in
  let rep = ref true in
  for i = 0 to d-1 do
    if (Subset.ins o1.z i && not (Subset.ins o2.z i)) ||
	 (Subset.ins o1.b i && not ( Subset.ins o2.b i))
    then
      rep := false;
    for j = 0 to d-1 do
      if not (less_eq_comp o1.comparison.(i).(j) o2.comparison.(i).(j)) then
	rep := false
    done
  done ;
  !rep


let lower t i1 i2 elt= 
  let comp = greatest_comp t.comparison.(i1).(i2) elt in
  t.comparison.(i1).(i2) <- comp;
  t.comparison.(i2).(i1) <- (op_comp comp)
(* we don't apply it to the Op as their
			      equivalence class are supposed to be
			      already computed *)

let check oop n =
  try
    Subset.iter 
      (fun i ->
       Subset.iter 
         (fun j ->
	  if ((not (Subset.ins oop.z i)) && 
                (not (Subset.ins oop.z j))
                && oop.comparison.(i).(j) <> Bot)
	  then raise Not_found
         )n
      )(Subset.comp n);
    true
  with |Not_found ->
         false


let concat t base letter =
  let d = t.size in
  let letter = Letter.double letter in
  let res = clone t in (* the result *)
  (match letter with
   | Letter.ParenT (u, b)
   | Letter.CrochetT (u, b) 
   | Letter.ChevronT (u, b) ->
      let z = Subset.comp (Subset.union u b) in
      res.b <- Subset.inter b t.b;
      res.z <- Subset.inter z t.z;
      let suc = Subset.inter u t.z in
      (* let u_less = Subset.minus u t.z in *)
      let pred = Subset.inter z t.b in
      (* let u_less = Subset.minus u_less t.b in *)
      let u' = Subset.union suc pred in
      let us = Subsets.inters t.u u in
      let us = Subsets.inters us u' in
      res.u <- us;
      Subset.iter 
        (fun pos_z ->
         Subset.iter
           (fun pos_u ->
            res.comparison.(pos_z).(pos_u)<- Les ;
            res.comparison.(pos_u).(pos_z)<- Gre
           ) u;
         Subset.iter
           (fun pos_b ->
            res.comparison.(pos_z).(pos_b)<- Les;
            res.comparison.(pos_b).(pos_z)<- Gre
           ) b
        )z;
      Subset.iter 
        (fun pos_u ->
         Subset.iter
           (fun pos_b ->
            res.comparison.(pos_u).(pos_b)<- Les;
            res.comparison.(pos_b).(pos_u)<- Gre
           ) b
        )u;
      Subset.iter 
        (fun i ->
         Subset.iter 
           (fun j-> res.comparison.(i).(j) <- Eq
           )u';
        )u'
   | Letter.Letter letter ->
      let seen = ref (Subset.empty d) in (* the set of variable we saw *) 
      for i = 0 to d-1 do
        let eq_i = ref(Subset.empty d) in (* the equivalence class of i*)
        let li = IntAr.get letter i in
        (match li, Subset.ins t.z i, Subset.ins t.b i with
         | 0, true, _ -> res.z <- Subset.add res.z i
         | x, _,true when x= base - 1 -> res.b <- Subset.add res.b i
         | _ ,_ ,_ ->());
        for j = 0 to d-1 do
          match li - IntAr.get letter j with 
          | 0 ->
	     let comp = t.comparison.(i).(j) in
             if comp = Eq 
             then (eq_i := Subset.add !eq_i j;
	           res.comparison.(i).(j)<- Eq)
             else  res.comparison.(i).(j) <- comp;
          | 1 when Subset.ins t.z i && Subset.ins t.b j ->
             eq_i:= Subset.add !eq_i j;
	     res.comparison.(i).(j)<- Eq
          | -1 when Subset.ins t.z j && Subset.ins t.b i ->
             eq_i := Subset.add !eq_i j;
	     res.comparison.(i).(j)<- Eq
          | x ->
             res.comparison.(i).(j) <- if x <0 then Les else Gre
        done ;
        if not (Subset.ins !seen i || Subset.ins t.z i || Subset.ins t.b i ) then
          (res.u <- Subsets.add res.u !eq_i;
           Subset.iter (fun i -> seen := Subset.add !seen i) !eq_i;)
      done
   | _  ->
      failwith "There is absolutely no reason to be here"
  );
  (* fprintf std_formatter "Concat of %a and %a is %a@." printf t Letter.printf letter printf res ; *)
  res




let gen_ex oop z b =
  let x= Array.make oop.size 0 in
  (* the subset of u in increasing  order according to comparison *)
  let subsets =
    List.sort 
      (fun left right ->
       let l= Subset.elt left
       and r = Subset.elt right in
       match oop.comparison.(l).(r) with
       | Les->  -1
       | Gre -> +1 
       | Eq | Bot -> 0
      )(Subsets.list oop.u)
  in
  let j= 
    List.fold_left
      (fun j set->
       Subset.iter 
         (fun i ->
          x.(i) <-
            if Subset.ins set i
            then j+1
            else j;
         ) set;
       j+2
      ) 2 subsets
  in 
  let bf = float_of_int b in
  (* the value b^m -1 with m as small as necessary *)
  let max = int_of_float (bf ** (floor ((log (float_of_int j))/. (log bf)+. 1.))) - 1
  in
  Subset.iter (fun i -> x.(i) <- max ) oop.b;
  Letter.int_to_word x b

(** takes two class is < js and return a new oop which make their union *)
let unite oop less gre =
  let i = Subset.elt less
  and j = Subset.elt gre
  and copy = clone oop
  and union = ref [] in
  Subsets.iter
    (fun ks ->
     let k = Subset.elt ks in
     (* if i<= k<= j*)
     if ( copy.comparison.(i).(k) = Les || copy.comparison.(i).(k) = Eq) &&
          ( copy.comparison.(k).(j) = Les || copy.comparison.(k).(j) = Eq)
     then 
       (union := ks :: !union;
        Subset.iter
          (fun k -> 
           copy.comparison.(i).(k) <- Eq;
           copy.comparison.(k).(i) <- Eq;
           copy.comparison.(j).(k) <- Eq;
           copy.comparison.(k).(j) <- Eq;
          ) ks
       )
    )oop.u;
  copy.u <- Subsets.unify oop.u !union ;
  copy

let formula_eq op z vars =
  let comps = ref [] in
  let d = op.size in
  for i = 0 to d -1 do
    for j = i+1 to d -1 do
      if ((not (Subset.ins op.z i)) && (not (Subset.ins op.z j))) then
        match op.comparison.(i).(j) with
        | Eq -> comps :=
                  (
                    match Subset.ins z i, Subset.ins z j with
                    | true, false -> Formula.succ (vars.(j),vars.(i))
                    | false, true -> Formula.succ (vars.(i),vars.(j))
                    | false, false | true, true -> Formula.equal (vars.(i),vars.(j))
                  ) :: !comps
        | _ -> ()
    done 
  done ;
  Subset.iter 
    (fun i ->
     comps := Formula.const (vars.(i),0) :: !comps
    ) op.z;
  Formula.and_list !comps

let is_u op i =
  let us = Subsets.union op.u in
  Subset.ins us i

let compare x y = 
  let r = ref 0 in
  let c = Subset.compare x.z y.z in
  if c<>0 
  then r := c;
  let c = Subset.compare x.b y.b in
  if c<>0 
  then r:=c;
  for i = 0 to x.size -1 do
    for j = 0 to y.size -1 do
      match  x.comparison.(i).(j), y.comparison.(i).(j) with
      | Eq, Eq | Les, Les| Gre, Gre| Bot, Bot-> ()
      | Eq, _ | _, Bot | Gre, Les-> r:= 1
      | _, Eq | Bot, _ | Les, Gre -> r:= -1
    done
  done;
  !r

module Comp =
  struct 
    type t = s
    let compare = compare 
end
module M = Map.Make (Comp)
module S = Set.Make (Comp)

let bot_eq t u=
  let r = ref true in
  for i = 0 to t.size-1 do
    for j = 0 to t.size-1 do
      match t.comparison.(i).(j), u.comparison.(i).(j) with
      | Bot, Bot -> ()
      | Bot, _  | _, Bot -> r:= false
      | _, _ -> ()
    done
  done;
  !r

let incomparable t u = 
  let r = ref Eq in
  try 
    for i = 0 to t.size-1 do
      for j = 0 to t.size-1 do
        match t.comparison.(i).(j), u.comparison.(i).(j) with
        | Eq, Eq | Gre, Gre | Les, Les | Bot, Bot -> ()
        | Eq, _ | _, Bot-> 
                   r := greatest_comp !r Gre;
                   if !r = Bot 
                   then
                     raise Math.Found
        | _, Eq | Bot, _-> 
                   r := greatest_comp !r Les;
                   if !r = Bot 
                   then
                     raise Math.Found
        | Les, Gre | Gre, Les ->
                      raise Math.Found
      done
    done;
    false
  with | Math.Found -> true
