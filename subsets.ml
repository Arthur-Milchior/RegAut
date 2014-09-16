open Format

(** the dimension, the union, the list of disjoing subset in increasing order according to compare *)

type t= int * Subset.t * (Subset.t_node Hset.t )


(** the empty set of size d*)
let empty d= (d, Subset.empty d, Hset.empty)

let partitions (d,_,l) =
  let rec aux acc union1 union2 const1 const2 todo =
    if Hset.is_empty todo
    then ((d,union1,const1), (d,union2,const2)) :: acc
    else 
      let h = Hset.choose todo in
      let t = Hset.remove h todo in
      let acc = aux acc (Subset.union h union1) union2 (Hset.add h const1) const2 t
      in        aux acc union1 (Subset.union h union2) const1 (Hset.add h const2) t
  in aux [] (Subset.empty d) (Subset.empty d) Hset.empty Hset.empty l
         
let enumerate (d,_,l) =
  Hset.fold 
    (fun u enumeration ->
     let enumeration =
       List.fold_left
         (fun acc (_,u',set) -> 
          let s = (d, Subset.union u u', Hset.add u set) in
          s:: acc
         ) enumeration enumeration
     in 
     enumeration
    ) l [(d,Subset.empty d, Hset.empty)]
    

let add (d,union,l) elt= 
  (d, Subset.union elt union, Hset.add elt l)
  
(** true if the set is empty *)
let is_empty (_,_,l) = Hset.is_empty l



let cut subset_list d=
  let add_if_non_empty set acc= 
    if Subset.is_empty set 
    then acc
    else set :: acc
  in
  (* take (a set SS of disjoint set, its union), and a set S.  Returns
     the non empty elements of S cut with set, and set minus S *)
  let rec aux (union, mutually_disjoints) set_to_add  =
    let union = Subset.union union set_to_add in
    (* This function add to disjoint_from_set the elements of
       not_disjoint minus the elements of sets, and set minus
       not_disjoint *)
    let rec aux2 (disjoint_from_set,remain) set_to_compare = 
      let (remain, center, right) = 
	Subset.cut_base remain set_to_compare 
      in
      let disjoint_from_set =
        add_if_non_empty right disjoint_from_set
      in 
      let disjoint_from_set =
        add_if_non_empty center disjoint_from_set
      in
      (disjoint_from_set, remain)
    in let disjoint, remain = 
         List.fold_left aux2 ([], set_to_add) mutually_disjoints in
       let disjoint = add_if_non_empty remain disjoint in
       (union,disjoint)
  in 
  let union,disjoint = List.fold_left aux (Subset.empty d,[]) subset_list
  in 
  let disjoint = Hset.of_list disjoint in
  (d,union,disjoint)

let inters (d, union, l) elt= 
  let add_if_non_empty set acc= 
    if Subset.is_empty set 
    then acc
    else Hset.add set  acc
  in
  let union = Subset.union elt union in
  let l,remain=
    Hset.fold 
      (fun elt (set,remain) ->
       let left,middle,remain = Subset.cut_base elt remain in
       let set = add_if_non_empty left set in
       let set = add_if_non_empty middle set in
       (set,remain)
      ) l (Hset.empty, elt)
  in
  let l = add_if_non_empty remain l in
  (d,union, l)

let union  (_, u, _)=u

let iter f (_,_,s)= Hset.iter f s

let inS s (_,_,l) =
  Hset.mem s l

let printf ppf (_,_,s) =
  Pretty.print_list 
    ppf  "@[{" ",@," "}@]" (Hset.elements s) Subset.printf

let list (_,_,l)  = (Hset.elements l)

let unify (d, u, subsets) set_to_unify =
  let newset = List.fold_left Subset.union (Subset.empty d) set_to_unify in
  let subsets = List.fold_right Hset.remove  set_to_unify subsets in
  let subsets = Hset.add newset subsets in
  (d,u,subsets)

