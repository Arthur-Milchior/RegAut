open Hashcons
open Format
(** the type of sets , v is the value, d the dimension *)
type t_node = { v:int; d:int} 
 and t = t_node hash_consed

let value x = x.node.v
module Comp= 
  struct
    type t = t_node
     let hash x = x.v
    let equal {v=v; d=d} {v=v'; d=d'}=  v=v' && d=d'
    let compare x y =
      let a= x.v - y.v 
      in if a = 0 then x.d -y.d 
         else a
  end

module H = Hashcons.Make (Comp)
let ht = H.create 251
let fc v = 
  H.hashcons ht v

let map = ref Hmap.empty 

(** the position of the first 1 in the binary representation *)
let min {node={v=v; d=d}} = 
  if v = 0 
  then d
  else
    let r = ref 0
    and x = ref v in
    while !x mod 2 = 0 do
      x:= !x lsr 1 ;
      incr r;
    done;
    !r

let empty d = fc {v=0; d=d}
let full d =
  let m = 1 lsl d in
  fc {v=m-1; d=d}


let enumerate_sub ({node={v=x; d=d}} as t ) =
  try
    Hmap.find t !map 
  with
  | Not_found ->
     (* take the set of position to change, the set to change and an
  accumulator*)
     let rec aux acc pos x =
       if pos = 0 
       then  (fc {v=x; d=d})::acc
       else 
         let i = pos land (-pos) (* the actual position *)
         and pos = pos land (pos -1) in (* the post without its rightmost element *)
         let acc = aux acc pos x in
         let x = x land (lnot i) in (* x without the position i*)
         aux acc pos x
     in 
     let subsets = aux [] x x  in
     map := Hmap.add t subsets !map;
     subsets
  
let enumerate d =
  let m = 1 lsl d in
  let l = Math.gen_list 0 (m-1) in
  List.rev_map (fun x -> fc {v= x; d=d}) l

let elt = min
let inter {node={v=v; d=d}}{node= {v=v'}}= fc {v=v land v'; d=d}
let union {node={v=v; d=d}}{node= {v=v'}}= fc {v=v lor v'; d=d}
let minus {node={v=x}} {node={v=y; d=d}} = fc {v=x land (lnot y); d=d}
let comp {node={v=v; d=d}} = 
  let m = (1 lsl d)-1 in
  fc {v = m land (lnot v); d=d}
let compare {node=u} {node=v} = Comp.compare u v

let is_subset x y = (minus x y).node.v = 0

let ins {node={v=x}} d =
  let i = 1 lsl d in
  i lor x = x 

let add {node={v=x;d=d}} i =
  let i = 1 lsl i in
  fc {v=i lor x ; d=d}

(* linear in the size of the set *)
let count {node={v=x}} =
  let x = ref x 
  and c= ref 0 in
  while !x<>0 do
    incr c;
    x:= !x land (!x-1) (*remove a 1*)
  done;
  !c

let is_disjoint {node={v=x}} {node={v=y}} = x land y =0

let fold f acc {node={v=x}} =
  let x = ref x 
  and acc = ref acc
  and i = ref 0 in
  while !x <> 0 do
    if !x mod 2 = 1 
    then acc := f !acc !i;
    incr i;
    x := !x lsr 1
  done;
  !acc 

let iter f x = fold  (fun _ x -> f x) () x

let to_list x =
  fold (fun acc x -> x :: acc ) [] x

let cut_base x y =
  minus x y, inter x  y, minus y x

let printf ppf x =
  Pretty.print_list ppf "<" "|" ">" (to_list x) (fun ppf i -> fprintf ppf "%d" i) 
let clone  x = x

let check_dim d {node={d= d'}} =
  d = d'

let is_empty {node={v=x}} = x= 0

let binary d x =fc {v=x; d=d}

let dimension {node=t} = t.d
