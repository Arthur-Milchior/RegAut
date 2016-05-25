open Hashcons
open Format
type comp = | Less | More | Eq | Mod of int
type set = | And | Or
type var =
  | Fr of int
  | Bounded of string *int * int
type quantifier =
  | Free
  | Existential
  | No_formula

module Comp =
  struct
  type t = var
  let compare = Pervasives.compare
end
module VS = Set.Make (Comp)

type right = 
  | Const of int (* = i *)
  | Minus of (int * var) (* +i-x *)
  | Var of (int * var) (* +i+x*)

module Comp' =
  struct
  type t = right
  let compare = Pervasives.compare
end
module RS = Set.Make (Comp')
module RM = Map.Make (Comp')



let not_comp = function
  | Less -> More
  | More -> Less
  | Eq -> Eq
  | Mod i -> Mod i

(**the strict comparison *)
let fcomps = function | Less -> (<) | More -> (>) | Eq -> (=) | Mod k -> (fun x y -> (x -y) mod k=0)
let fcomp = function | Less -> (<=) | More -> (>=) | Eq -> (=) | Mod k -> (fun x y -> (x -y) mod k=0)


type neg = | Pos | Neg
let not_neg = function | Pos -> Neg | Neg -> Pos

let printf_comp neg ppf c = 
  let c= 
    if neg= Neg
    then
      match c with
      | Eq -> "≠" 
      | Less -> ">" 
      | More -> "<"
      | Mod _ -> "≭"
    else
      match c with
      | Eq -> "=" 
      | Less -> "≤" 
      | More -> "≥"
      | Mod _ -> "≡"
  in
  fprintf ppf "%s" c

let vid = ref (-1)
(** The type of variable, it must correspond to a position *)


let newBoundVar s= 
  incr vid;
  Bounded (s,0, !vid)

let newBoundArray d s=
  incr vid;
  Array.init d (fun i -> Bounded (s^ string_of_int i,i, !vid))


let free d =
  let list_0_d = Array.of_list (Math.gen_list 0 (d -1)) in
  Array.map (fun i -> Fr i) list_0_d


type t =  t_node Hashcons.hash_consed
and t_node =
  | Imply of ( t*t)
  | Set of t_node Hset.t * set
  | Not of t
  | True | False
  | Bool of var
  | Comp of (var* right * comp)
  | Assert of t

(*type sigma1 = (var list *  t))*)
module For_node =
  struct 
    type t = t_node
    let equal f g= 
      match (f,g) with
      | Set (l,a), Set (m,b) -> Hset.equal l m && a==b
      | Assert a , Assert b
      | Not a , Not b -> a==b 
      | Imply (a,b) , Imply (c,d) -> a==c && b ==d
      | Bool a , Bool b -> a=b
      | Comp (a,r,c), Comp (b,s,d)  -> a=b && r = s && c=d
      | False, False | True, True -> true
      | _, _ -> false

    let hashSet s = 
      Hset.fold (fun e acc-> 2* acc + e.hkey) s 0

    let hash f =
      abs(match f with 
      | Imply (l,r) -> 2* l.hkey + 3* r.hkey
      | Set (f,And) -> 3* (hashSet f)
      | Set (f,Or) -> 5* (hashSet f)
      | Not f -> 7 * f.hkey
      | Assert f -> 11 * f.hkey
      | f -> Hashtbl.hash f)

  end

module Fterm = Hashcons.Make(For_node)
let ft = Fterm.create 251

(** a function to obtain a t from a t_node *)
let fc f=
  Fterm.hashcons ft f

let true_ = fc True
let false_ = fc False


let zero = function | And -> false_ | Or -> true_
let unity = function | And -> true_ | Or -> false_
let lesseq (a, b) = if a=b then true_ else fc (Comp (a,Var(0,b), Less))
let lessst (a, b) = if a=b then false_ else fc (Comp (a,Var(-1,b),Less))
(** a <= i +b *)
let less (a,i, b) = if a=b then false_ else fc (Comp (a,Var(i,b),Less))
(** x <= i*)
let lessc (x,i) = fc (Comp (x,Const(i),Less))
(** x>= i*)
let morec (x,i) = fc (Comp (x,Const(i),More))
let bool a = fc (Bool a)
(** x = i*)
let const (x,i)= fc (Comp(x,Const(i),Eq))

let comp (a,b,c) = 
  match b, c with
  | _, Mod k when k = 1 -> true_ 
  | Const i, Mod k -> fc (Comp (a, Const (Math.mod_ i k), Mod k))
  | Var (i, x), Mod k -> 
     if x = a then 
       if i mod k = 0 
       then true_
       else false_
     else
       fc (Comp (a, Var (Math.mod_ i k, x), Mod k))
  | Minus (i, x), Mod k ->
     if a= x 
     then 
       (* x= i-x mod k; 2x = i mod k; if k and i are even; x = i/2 mod k/2; else x = i(2^-1) mod k *)
       if k mod 2 = 0 
       then if i mod 2 = 0
            then fc (Comp (a, Const (i/2), Mod (k/2)))
            else false_
       else 
         let two = Math.mul_inv 2 k in
         let i = Math.mod_ (i*two) k in
         fc (Comp (a, Const i, Mod k))
     else
       fc (Comp (a, Minus (Math.mod_ i k, x), Mod k))
  (* a ~ i+a; 0 ~ i;*)
  | Var (i,x), _ when a = x->
     if (fcomp c) 0 i then true_ else false_
  (* a ~ i-a; 2a ~ i; a~ i/2 *)
  | Minus (i,x), Eq when a = x->
     if i mod 2 = 0
     then fc (Comp (x, Const (i/2), Eq))
     else false_
  | Minus (i,x), Less when a = x->
     fc (Comp (x, Const (Math.half_floor i), Less))
  | Minus (i,x), More when a = x->
     fc (Comp (x, Const (Math.half_ceil i), More))
  |  _ ->fc (Comp (a,b,c))

let mod_ (x,i,k) = comp(x, Const i, Mod k)
let mode (x,i,y,k) = comp (x, Var(-i,y), Mod k)
let add (a, b, c) = comp (a, Var(-b, c), Eq)

(** x = b-y *)
let minus (x, b, y) = comp (x,Minus( b, y), Eq)

let set (s, typ) =
  let s = 
    Hset.fold 
      (fun e acc ->
       match
            e.node, typ, acc with
       |         _,   _, None 
       |     False, And, _
       |      True,  Or, _
       |  Assert _,  Or, _ ->  None
       |Set (s, a),   _, Some acc  when a = typ -> Some (Hset.union s acc)
       |      True, And, _
       |     False,  Or, _ -> acc
       |         _,   _, Some acc ->  Some (Hset.add e acc)
      ) s (Some Hset.empty) in
  match s with 
  | None -> zero typ
  | Some s->
     if Hset.is_empty s 
     then unity typ 
     else if Hset.is_singleton s
     then Hset.choose s
     else fc (Set(s, typ))

let and_ s = set (s,And)
let or_ s = set (s,Or)

let set_list typ l=
  let l = List.fold_left (fun set e -> Hset.add e set) Hset.empty l in
  set (l, typ)

let or_list = set_list Or
let and_list = set_list And

let rec not_ f= 
  match f.node with
  | Not f -> f
  | False -> true_
  | True -> false_
  (* x > i-y; x>= i+1-y*)
  | Comp (x,Minus(i, y), Less) -> comp (x, Minus(i+1,y), More)
  | Comp (x,Var(i, y), Less) -> comp (x, Var(i+1,y), More)
  (* x < i-y; x<= i-y-1*)
  | Comp (x,Minus(i, y), More) -> comp (x, Minus(i-1,y), Less)
  | Comp (x,Var(i, y), More) -> comp (x, Var(i-1,y), Less)
  (* x>i; x>=i+1*)
  | Comp (x,Const( i), Less) -> morec (x, i+1)
  (* x<i; x<=i-1*)
  | Comp (x,Const( i), More) -> lessc (x, i-1)
  | Comp (x, r, Mod 2) ->
     let r = 
       match r with 
       | Const i -> Const (i+1)
       | Minus (i, y) -> Minus (i+1, y)
       | Var (i,y) -> Var (i+1, y)
     in comp (x, r, Mod 2)
  | _ -> fc (Not f)

let imply (l,r) = 
  match l.node, r.node with 
  | False, _ -> true_
  | _, True -> true_
  | True, _ -> r
  | _, False -> not_ l
  | _, _ -> fc (Imply (l, r))

(** mod x =c+ y mod k using only mod predicate *)
let mod_lin (x, c, y, k) =
  let list = Math.gen_list 0 (k-1) in
  let a= 
    List.map
      (fun i->
       let l= mod_ (x, i, k) 
       and r = mod_ (x, i -c, k) in
       imply (l,r)
      ) list
  in and_list a

(** mod x =+c -y mod k using only mod predicate *)
let mod_lin_n (x, c, y, k) =
  let list = Math.gen_list 0 (k-1) in
  let a= 
    List.map
      (fun i->
       let l= mod_ (x, i, k) 
       (* y = c-x*)
       and r = mod_ (x, c-i, k) in
       imply (l,r)
      ) list
  in and_list a

(** x+1 = y*)
let equal (x, y) = add (x,0,y)
let succ (x, y) = add (x,1,y)

let neg x = lessc (x,-1)

(** remain the value of the free variable, raise Invalid_argument if it is bounded *)
let of_free =  function
  |  Fr n -> n
  | _ -> raise (Invalid_argument "not a free variable")


let rec length f= 
  let lv = function
    | Fr x | Bounded (_, x,_)-> Math.log x
  in
  let lr = function
    | Const i -> Math.log i
    | Var (i,x)
    | Minus (i,x)->
       lv x + Math.log i
  in
  let lc = function
    | Less | More | Eq -> 1
    | Mod k -> Math.log k
  in
  match f.node with
  | Imply (l,r) -> 1+ length l+ length r
  | Set (f,_) -> Hset.fold (fun x i -> (length x) + i + 1) f (-1)
  | Assert f
  | Not f -> 1 + (length f)
  | Comp (x,r,c) -> lv x + lc c +lr r
  | True | False -> 1
  | Bool v -> lv v


let eq_tuple ar1 ar2 =
  let ar1= Array.to_list ar1
  and ar2 = Array.to_list ar2 
  in and_list (List.map2 (fun i j -> equal (i,j)) ar1 ar2 )


(** return a set of variable appearing once/many time*)
let mod_bound1_many f =
  let add i (one, two) = 
    match i with 
    | Bounded _ -> 
       if VS.mem i two
       then (one, two)
       else
         if VS.mem i one 
         then (VS.remove i one), (VS.add i two)
         else (VS.add i one), two
    | Fr _ -> (one,two) in
  let add_right r bs = match r with
    | Minus (_, x)
    | Var (_, x) -> add x bs
    | Const _ -> bs in
  let add_comp k = function
    | Mod k' -> Math.lcm k k'
    | _ -> k in
  let rec aux f ((k,bs) as acc)= match f.node with
    | Imply (l,r) -> aux r (aux l acc)
    | Set (l,_)  -> Hset.fold aux l acc
    | False | True -> acc
    | Assert f -> acc
    | Not f -> aux f acc
    | Comp (v,r,c) -> add_comp k c, add_right r (add v bs)
    | Bool v -> k, add v bs
  in aux f (1,(VS.empty,VS.empty))

let printf_var ppf  = function
  | Fr i -> fprintf ppf "x%d" i
  | Bounded (s , _, _ ) -> fprintf ppf "%s" s 

let printf_right ppf r = 
  match r with 
  | Const i -> fprintf ppf "%d" i
  | Minus (i,v)-> 
     if i = 0 then 
       fprintf ppf "-%a" printf_var v
     else
       fprintf ppf "%d-%a" i printf_var v
  | Var (i,v)-> 
     if i = 0 then 
       fprintf ppf "%a" printf_var v
     else
       fprintf ppf "%d+%a" i printf_var v

let printf ppf formula =
  let rec aux pff f =
    match f.node with
    | Imply (l, r) -> 
       fprintf ppf "@[(%a⇒@ %a)@]" aux l aux r
    | Set (phis, And) ->
       if Hset.is_empty phis
       then fprintf ppf "(∧)"
       else Pretty.print_set ppf "@[(" "∧@ " ")@]" phis aux
    | Set (phis, Or) ->
       if Hset.is_empty phis
       then fprintf ppf "(∨)"
       else Pretty.print_set ppf "@[(" "∨@ " ")@]" phis aux
    | True -> fprintf ppf "True"
    | False -> fprintf ppf "False"
    | Bool x -> printf_var ppf x
    | Comp (x, v, c) -> 
       fprintf ppf "%a%a%a" printf_var x (printf_comp Pos) c printf_right v;
       (match c with | Mod k -> fprintf ppf " mod %d" k| _ -> ())
    | Assert f -> fprintf ppf "[%a]" aux f
    | Not phi->
       match phi.node with
       | Comp (x,v, c) -> 
          fprintf ppf "%a%a%a" printf_var x (printf_comp Neg) c printf_right v;
          (match c with | Mod k -> fprintf ppf " mod %d" k| _ -> ())
       | _ -> fprintf ppf "¬%a" aux phi
  in 
  aux ppf formula

let assert_ f = 
  (* fprintf std_formatter "We assert %a@." printf f; *)
  match f.node with
  | False -> false_
  (* the case true should never happen, but who knows, it wouldn't be
  mathematicaly false *)
  | True -> true_  
  | _ -> fc (Assert f)

let power bm =
  let pr = function
    | Const i -> Const (i*bm)
    | Var (i,x) -> Var (i*bm, x)
    | Minus (i,x) -> Minus (i*bm, x)
  in
  let pc = function
    | Mod k -> Mod (k*bm)
    | x -> x
  in
  let rec aux f= match f.node with
    | Imply (l,r) -> imply (aux l, aux r)
    | Set (f,a) -> set (Hset.map aux f, a)
    | Not f -> not_ (aux f)
    | Comp (x,r,c) -> comp (x, pr r, pc c)
    | True -> true_ | False -> false_
    | Bool v -> bool v
    | Assert f -> assert_ (aux f)
  in aux 

let move i f =
  let replace j = let j = of_free j in Fr (if j<i then j else j+1) in
  let replace_right = function
    | Var (i,y) -> Var (i, replace y)
    | Minus (i,y) -> Minus (i, replace y)
    | Const (i) -> Const i
  in
  let rec aux f = match f.node with
    | Imply (l, r) -> 
       imply (aux l, aux r)
    | Set (l, a) -> 
       set ((Hset.map aux l),a)
    | Assert f -> assert_ (aux f)
    | Not f -> not_ (aux f)
    | Comp (x,r, c) -> 
       comp (replace x,replace_right r,c)
    | Bool x -> bool (replace x)
    | True -> true_
    | False -> false_
  in aux f

         
let change_free bound f =
  let replace  =  function 
    | Fr x -> bound.(x)
    | x -> x in
  let replace_right = function
    | Const i -> Const i
    | Minus (i, v) -> Minus (i, replace v)
    | Var (i, v) -> Var (i, replace v) in
  let rec aux f = match f.node with
    | Assert f -> assert_ (aux f)
    | Imply (l,r) -> imply (aux l, aux r)
    | Set (l,a) -> set (Hset.map aux l, a)
    | True -> true_
    | False -> false_
    | Not f -> not_ (aux f)
    | Bool x -> bool (replace x)
    | Comp (x,r, c)-> comp (replace x,replace_right r, c)
  in aux f


(**return a shorter equivalent formula*)
(*neg is Neg if there is an odd number of Negation before the variable. 
If neg true, then more set are accepted when a predicate is replaced by "false" else by "true"
And if a variable is used only once, we can always assume it is in an atomic formula which is as we want true or false *)
let rec simplify natural formula=
  (* Take a fact that we assume (or whose negation we assume) and simplify according to it *)
  let rec change fact assumed_true formula = 
    let rec aux formula =
      (* fprintf std_formatter "we replace %a in %a @." printf fact printf formula; *)
      if formula == fact
      then (
        (* fprintf std_formatter "we have replaced %a@." printf fact; *)
        if assumed_true then true_ else false_)
      else match formula.node with
           | Imply (l,r) -> imply (aux l,aux r)
           | Set (l,a) -> set (Hset.map aux l, a)
           | Not f -> not_ (aux f)
           | Assert f -> let f' = aux f in
                         (* if f.node = False  *)
                         (* then fprintf std_formatter "we have changed %a in False@." printf f; *)
                         assert_ (f')
           | Comp (x,Const i, Eq) -> (*x = i *)
              (match fact.node with
               | Comp (y,Const j,Mod( k)) when x=y  ->
                  (* x = j mod k;*)
                  if Math.xor ((j -i )mod k = 0) assumed_true
                  (* if we know that x = j mod k, and we state that x = i != j mod k, then false *)
                  (* if we know that x != j mod k, and we state that x= i = j mod k, then false *)
                  then (
                    (* fprintf std_formatter "simplification of %a with %a, j=%d, i=%d, mod=%d@." printf formula printf fact j i ((j-i) mod k); *)
                    false_)
                  else formula
               | Comp (y,Const( j), Eq) when x=y && assumed_true -> (*we can assume that i<>j *)
                  false_
               | _ -> formula)
           | Comp (x,Const(i),Mod(k)) ->
              (match fact.node with
               | Comp (y,Const(j),Mod(k')) when k=k' && x=y ->
                  (*we can assume that i<>j *)
                  if assumed_true
                  then false_
                  else formula
               | _ -> formula
              )
           | Comp (x,Const(c),e) -> (* e is Less or More, let say Less; x<= c *)
              (match fact.node with
               | Comp (y,Const(i), Eq) when x=y -> (* we know that x=i*)
                  if assumed_true && (fcomp e) i c
                  then true_
                  else formula
               | Comp (y,Const(i), f) when x=y && (not_comp f)=e -> (* x >=i *)
                  if (fcomps e) c i && assumed_true (* if c<i *)
                  then false_
                  else formula
               | Comp (y,Const(i), f) when x=y && f=e && assumed_true -> (* x <=i *)
                  let extremum = if (fcomp e) c i(* if c<i*) then c else i in
                  comp (x, Const extremum, f)
               | _ -> formula
              )
           | Comp (x,Var(i,y), Eq)-> (* x = i+y *)
              (match fact.node with
               | Comp (z,Const( j), Eq)  when x=z && assumed_true-> (* x = j*)
                  const (y,j-i)
               | Comp (z,Const( j), Eq)  when y=z && assumed_true -> (*y=j*)
                  const (x, j+i)
               | _ -> formula)
           | Comp (x,Minus(i,y), Eq)-> (* x = i-y *)
              (match fact.node with
               | Comp (z,Const( j), Eq)  when x=z && assumed_true-> (* x = j*)
                  const (y,i-j)
               | Comp (z,Const( j), Eq)  when y=z && assumed_true -> (*y=j*)
                  const (x, i-j)
               | _ -> formula)
           | _ -> formula
    in 
    match fact.node, assumed_true with 
    | Not fact, _ -> change fact (not assumed_true) formula
    | Assert fact, _ -> formula
    | Comp (a, b, Less), false 
    | Comp (a, b, More), false -> change (not_ fact) true formula
    | _ ->  aux formula
  in
  let _,(bounded_once, _)= mod_bound1_many formula in
  (* the actual simplification *)
  let rec aux neg formula= 
    let if_neg =(if neg= Neg then false_ else true_) in
    match formula.node with 
    | True -> true_
    | False -> false_
    | Comp (_,_,Mod k) when k=1 -> true_
    | Assert f -> let f' = (aux neg f) in
                  (* if f.node = False  *)
                  (* then fprintf std_formatter "we have simplified %a in False@." printf f; *)
                  assert_ f'

    | Comp (x,Const(_),c) -> 
       let b1 = VS.mem x bounded_once in
       if b1 
       then
         if natural = Math.Z
         then if_neg
         else  
           match
             c, neg with
           |   Eq,   _ -> if_neg
           |Mod _,   _ -> if_neg
           | More, Pos -> true_
           | Less, Neg -> false_
           |    _,   _ -> formula
       else formula

    (* x ~ i + y *)
    | Comp (x,Var( i,y), c) ->
       let x_once= VS.mem x bounded_once
       and y_once = VS.mem y bounded_once in
       if (not x_once) && (not y_once) 
       then formula
       else (
         if natural= Math.Z 
         then if_neg
         else
           match 
             x_once, y_once, neg,    c, i<0,  i>0 with
           |  false,  false,   _,    _,    _,   _ -> formula
           |      _,      _,   _,Mod _,    _,   _ 
           |   true,   true,   _,    _,    _,   _  -> if_neg
           |   true,      _, Pos,   Eq,false,   _
           |      _,   true, Pos,   Eq,    _, false
           |   true,      _, Pos, More,    _,   _  
           |      _,   true, Pos, Less,    _,   _  -> true_
           |   true,      _, Neg, Less,    _,   _  
           |      _,      _, Neg, Eq  ,    _,   _ 
           |      _,   true, Neg, More,    _,   _  -> false_
           |      _,      _,   _,    _,    _,   _ -> formula)

    (* x ~ i-y *)
    | Comp (x,Minus( i, y), c) ->
       let x_once = VS.mem x bounded_once
       and y_once = VS.mem x bounded_once in
       if (not x_once) && (not y_once) 
       then formula
       else (
         if natural = Math.Z
         then if_neg 
         else 
           match (*if x_once is false, then y_once *)
             x_once, neg,   c with
           |  true,  Pos, More
           | false,  Pos, Less -> true_
           |  true,  Neg, Less
           | false,  Neg, More -> false_
           |     _,    _,    _ -> formula)
    | Bool x -> 
       if VS.mem x bounded_once then if_neg else formula

    | Imply (l, r) -> 
       let l = aux (not_neg neg) l
       and r = aux neg r
       in 
       let r= change l true r in
       let l = change r false l in
       imply (l, r)
    | Not formula ->
       let formula = aux (not_neg neg) formula in
       not_ formula

    | Set (s,a) -> 
       let s = Hset.map (aux neg) s in
       (* this function takes the list of formula already seen and the
       one to do *)
       let rec aux2 seen todo =
         if Hset.is_empty todo
         then seen
         else 
           let f = Hset.choose todo in
           let todo = Hset.remove f todo in
           let change_set =  Hset.map (change f (a = And))  in
           let seen = change_set seen in
           let todo = change_set todo in
           let seen = Hset.add f seen in
           aux2 seen todo
       in
       let s = aux2 Hset.empty s in
       set (s,a)
  in
  let formula' = aux Pos formula in 
  (* if formula = formula' *)
  (* then *)
  (*   fprintf std_formatter "we can't simplify %a@.@." printf formula *)
  (* else *)
  (*   fprintf std_formatter "we simplify %a in@. %a@.@." printf formula printf formula'; *)
  (* the algorithm is a fixpoint, we repeat while things change. It
  ends, because it change to become smaller *)
  if formula != formula' then simplify natural formula' else formula'

let simplify_natural = simplify Math.N
let simplify_relative = simplify Math.Z

let inN n =function
  | Bounded (_, x, _) | Fr x -> Subset.ins n x

(*could take inN as a parameter if needed*)
let change_n n = 
  let rec aux formula = match formula.node with
    | Imply (l,r) -> imply (aux l, aux r)
    | Assert f -> assert_ (aux f)
    | Set (f,a) -> set (Hset.map aux f, a)
    | Not f -> not_ (aux f)
    | Comp (x,Const i,c) -> if inN n x (*-x-1 <=i; x+1>=-i*)
                            then comp (x, Const(-i-1), not_comp c) 
                            else formula
    | Comp (x, Minus(i, y), c) -> 
       (match inN n x, inN n y  with 
        (*-x-1  <= i -(-y-1); x +1>= -i-y-1; x>=-y -i -1 *)
        | true, true->  comp (x, Minus (-i -1, y), not_comp c)
        | false,false-> formula
        (* x <= i-(-y-1); x<=y +i +1*)
        | false, true -> comp (x,Var(i+1,y),c)
        (* -x-1 <= i-y; x+1>= y-i; x>= y-i-1*)
        | true, false -> comp (x,Var(-1-i,y), not_comp c))
    | Comp (x, Var(i, y), c) -> 
       (match inN n x, inN n y  with 
        (*-x-1  <= i +(-y-1); x +1>= -i+y+1; x>=+y -i +1 *)
        | true, true->  comp (x, Var (-i +1, y), not_comp c)
        | false,false-> formula
        (* x <= i+(-y-1); x<=-y +i -1*)
        | false, true -> comp (x,Minus(i-1,y),c)
        (* -x-1 <= i+y; x+1>=-y-i; x>=-y-i-1*)
        | true, false -> comp (x,Var(-1-i,y), not_comp c))
    | True | False | Bool _ -> formula
  in aux

let rec add1 f =
  match f.node with
  | Imply (l,r) -> imply (add1 l, add1 r)
  | Set (f,a) -> set (Hset.map add1 f,a)
  | Not f -> not_ (add1 f)
  | Assert f -> assert_ (add1 f)
  (*x-1 ~ i; x ~i+1 *)
  | Comp (x,Const( i),c) -> comp (x, Const(i+1),c)
  (* x-1 ~ i-(y-1); x ~ i+2 -y*)
  | Comp (x,Minus(i,y),c) -> comp (x, Minus(i+2, y),c)
  | Comp (_, Var _, _) 
  | True | False | Bool _ -> f


(** replace Bool x by value in f*)
let rep_bool x f value = 
  let rec aux f = 
    match f.node with 
    | Imply (l, r) -> imply (aux l, aux r)
    | Set (fs,a) -> set (Hset.map aux fs,a)
    | Not f -> not_ (aux f)
    | Assert f -> assert_ (aux f)
    | Bool y -> if x = y
                then value
                else f
    | True | False | Comp _ -> f
  in aux f

(** "add_var i r" add the integer i to the variable r*)
let add_var i =function
  | Const j -> Const (i+j)
  | Var (j,y) -> Var (i+j,y)
  | Minus (j,y) -> Minus (i+j,y)

(** add - to the constant part *)
let inv_var  =function
  | Const j -> Const (-j)
  | Var (j,y) -> Var (-j,y)
  | Minus (j,y) -> Minus (-j,y)

(** replace_var x val1 val2, replace the occurence of x in the "right"
val1 by val2 and return a new right argument *)
let replace_var x val1 val2 = 
  match val1 with
  | Var (i, y) -> if x=y
                  then add_var i val2
                  else Var (i,y)
  | Minus (i, y) -> if x = y 
                   then add_var i (inv_var val2)
                   else Minus(i,y)
  | Const i -> Const i

(** as comp, but left take a "right" instead of a variable*)
let rec comp_left (l, r, c) =
  match l, r with
  | Const i, Const j-> 
     if (fcomps c) i j 
     then true_
     else false_
  (* i< j+x; x>i-j*)
  | Const i, Var (j,x) ->
     comp (x, Const (i-j), not_comp c)
  (* i < j -x; x< j-i *)
  | Const i, Minus (j,x) ->
     comp (x, Const (j-i), not_comp c)
  (* i-x < j-y; x-i> y-j *)
  | Minus (i, x), Minus (j,y) -> comp_left (Var (-i, x), Var (-j,y), not_comp c)
  | Minus _, _  -> comp_left (r,l,not_comp c)
  | Var (i, x), _ -> comp (x, add_var (-i) r , c)

(** replace every occurence of the bounded variable x in f with the value "right"*)
let rep_right x f value = 
  let rec aux f = 
    match f.node with 
    | Imply (l, r) -> imply (aux l, aux r)
    | Set (fs,a) -> set (Hset.map aux fs,a)
    | Not f -> not_ (aux f)
    | Assert f  -> assert_ (aux f)
    | True | False | Bool _ -> f
    | Comp (y, r, c) ->
       let r = replace_var x r value 
       in
       if x = y 
       then comp_left (value, r, c)
       else comp (y,r,c)
  in aux f

(** return a set of value that is compared to the variable, or None if it is a boolean *)
let find_comp var f=
  let add i x acc = 
    if x = var then RS.add i acc else acc
  in
  let right r x acc =match r with
    | Const _ -> acc
    | Minus (i, y) ->
       (* x = i -y; hence y = i-x*)
       add (Minus (i, y)) y acc
    | Var (i, y) -> 
       (* x= i + y; hence y  = x-i*) 
       add (Var (-i,x)) y acc 
  in
  let rec aux f acc = 
    match acc with
    | None -> None
    | Some acc' ->
       match f.node with
       | Imply (l,r) -> aux r (aux l acc)
       | Set (l,_)  -> Hset.fold aux l acc
       | Not f -> aux f acc
       | True | False | Assert _
       | Comp (_, _, Mod _) -> acc
       | Comp (v, r, _) -> Some( right r v (add r v acc'))
       | Bool x-> if x = var then None else acc
  in aux f (Some RS.empty)

let elim natural f = 
  let k, (vars1, vars2) = mod_bound1_many f in
  (* fprintf std_formatter "k is %d@." k; *)
  let vars = VS.union vars1 vars2 in
  let vars = VS.filter (function | Bounded _ -> true | Fr _ -> false) vars in
  let f'=
    VS.fold 
      (fun bounded_var f ->
       let f = simplify natural f in
       let vars = find_comp bounded_var f in
       match vars with 
       | None ->
          ((* fprintf std_formatter "replacing %a by bools@." printf_var bounded_var; *)
            or_list [rep_bool bounded_var f true_; rep_bool bounded_var f false_])
       | Some vars ->
          let vars = 
            if RS.is_empty vars  (* if we don't compare it to constant, we must choose arbitrarily one *)
            then RS.singleton (Const 0)
            else vars
          in
          let all_vars = ref RS.empty in
          RS.iter 
            (fun v->
             match v with 
             | Const i ->
                let min = if natural = Math.N
                          then max 0 (i-k)
                          else i -k 
                in
                for j = min to i+k do
                  all_vars := RS.add (Const j) !all_vars
                done;
             | Var (i,x) ->
                for j = i-k to i+k do
                  all_vars := RS.add (Var (j,x)) !all_vars
                done;
             | Minus (i,x) ->
                for j = i-k to i+k do
                  all_vars := RS.add (Minus (j,x)) !all_vars
                done;
            ) vars;
          let vars = RS.elements !all_vars in
          let fs = List.map 
                     (fun v -> 
                      let f'=rep_right bounded_var f v in
                      (* fprintf std_formatter "@.replacing %a@, by %a@, in %a@, we get %a@." printf_var bounded_var printf_right v printf f printf f'; *)
                      f'
                     ) vars in
          let f'= or_list fs in
          f'
      ) vars f
  in
  (* fprintf std_formatter "%a@,, without quantifier is %a@." printf f printf f'; *)
  simplify natural f'

let rec remove_assert f =
  let rec aux f= 
    match f.node with
    | Imply (l,r) -> imply (aux l, aux r)
    | Set (f,a) -> set (Hset.map aux f, a)
    | Not f -> not_ (aux f)
    | Assert f -> true_
    | _ -> f
  in
  let f' = aux f in
  (* fprintf std_formatter "removing in %a@, gives %a@." printf f printf f'; *)
  f'

let remove_simplify n f= 
  simplify n (remove_assert (simplify n f))

(** compute the residual of the formula, with the letter s in base
    b*)
let residual b s f=
  let of_s x = let x' = of_free x in IntAr.get s x' in
  let rec aux f =
    match f.node with
    | Imply (l, r) -> imply (aux l, aux r)
    | Set (e,a) -> set (Hset.map aux e,a)
    | Not f -> not_ (aux f)
    | Assert f -> assert_ (aux f)
    | True | False | Bool _ -> f
    | Comp (x, r, c) ->
       let g = ref 0 in
       let of_r = match r with
         | Var (i,y)  -> i+ of_s y
         | Minus (i, y) -> i- of_s y
         | Const i -> i
       in
       let num = of_r - (of_s x) in
       let num = match c with 
         | Mod k -> 
            g := Math.gcd b k;
            let b'= b/ !g in
            let k'= k/ !g in
            let inv =Math.mul_inv b' k' in
            num * inv
         | _ -> num
       in
       let num_b = match c with 
         | More -> Math.div_ceil num b 
         | Less -> Math.div_floor num b 
         | Eq  ->  num / b 
         | Mod k -> num / !g
       in
       let r = match r with 
         | Var (_,x) -> Var (num_b,x)
         | Minus (_,x) -> Minus (num_b,x)
         | Const _ -> Const num_b
       in
       let f = comp (x, r, c) in
       match c with 
       | Eq -> 
          if num mod b = 0
          then f
          else false_
       | Mod k ->
          if num mod !g =0 
          then comp (x, r, Mod (k/ !g))
          else false_
       | Less| More -> f
  in aux f

(** compute the inverse of the residual of the formula,  with the letter s in base b*)
let inv_residual b s f=
  let rec aux f =
    match f.node with
    | Imply (l, r) -> imply (aux l, aux r)
    | Set (e,a) -> set (Hset.map aux e,a)
    | Not f -> not_ (aux f)
    | Assert f -> assert_ (aux f)
    | Bool x -> bool x
    | True -> true_
    | False  -> false_

    | Comp (x, r, c) ->
       let x' = of_free x in
       let sx = (IntAr.get s x')in
       let c = match c with
         | Mod k -> Mod (k*b)
         | _ -> c
       in
       let r = 
         match r with
         | Const i -> Const (i*b + sx)
         | Var (i,y) -> 
            let y' = of_free y in
            let sy = (IntAr.get s y')in
            Const (i*b + sx -sy)
         | Minus (i,y) -> 
            let y' = of_free y in
            let sy = (IntAr.get s y')in
            Const (i*b + sx + sy)
       in 
       comp (x, r, c)
  in
  let f' = aux f in
  (* fprintf std_formatter "inv_residual of %a@, by %a@,, is %a@." printf f IntAr.printf s printf f'; *)
  f'

let rec accept_n n f =
  let inN = inN n in
  let val_var x = (if inN x then -1 else 0) in
  let rec aux f =
    match f.node with
    | Imply (l,r) -> aux r || (not (aux l))
    | Set (f,a) ->
       Hset.fold 
         (fun f  ->
          (match a with And -> (&&) | Or-> ( || )) (aux f)
         ) f (a = And)
    | Not f -> not (aux f)
    | Assert f -> aux f(* this shouldn't happen but who knows *)
    | False -> false
    | True -> true
    | Bool v -> true
    | Comp (x,r, c) ->
       let val_right = match r with
         | Const i -> i
         | Var (i, x) -> i + (val_var x)
         | Minus (i, x) -> i - (val_var x)
       in 
       (fcomp c) (val_var x) val_right
  in aux f

(** generate an automaton from the formula, right now, assuming positive value *)
let to_aut b d set f= 
  let map = ref Hmap.empty in
  let f = elim Math.N f in
  let id = ref 0 in 
  let abd = IntAr.enum b d in
  let rec aux n f =
    try 
      let t, l= Hmap.find f !map in
      try 
        List.assoc n !l 
      with 
      | Not_found ->
         (let this_id = ! id in
          incr id;
          l:= (n,this_id) :: !l;
          this_id)
    with
    | Not_found->
       (* fprintf std_formatter "generating %a@." printf f; *)
       let this_id = !id in
       incr id ;
       let delta = ref IntAr.M.empty in
       map := Hmap.add f (delta,ref [n,this_id]) !map;
       List.iter
         (fun a ->
          let n = 
            if set = Math.Z 
            then 
              let n= ref(Subset.empty d) and ok = ref true in
              for i = 0 to d-1 do
                let ai = IntAr.get a i in
                if ai =b-1 then n:= Subset.add !n i
                else if ai>0 then  ok:= false
              done ;
              (if !ok then Some !n else None)
            else None
          in
          let f' = residual b a f in
          delta := IntAr.M.add a (aux n f') ! delta
         ) abd;
       this_id
  in
  ignore(aux None f);
  let states = Array.make !id Simple.state_dummy in
  Hmap.iter 
    (fun f (delta,ns) ->
     List.iter 
       (fun (n,i) ->
        let final =
          match n, set with
          | _, Math.N -> accept_n (Subset.empty d ) f 
          | None, _ -> false
          | Some n , _ -> accept_n n f
        in
        let name1 = Pretty.string printf f in
        let name2 = match n, set  with 
          |None, _ | _, Math.N -> ""
          | Some n, _ -> Pretty.string Subset.printf n
        in
        let name = name1^name2 in
        let state = {Simple.delta= !delta; Simple.id = i; Simple.final = final; Simple.nameS=name}
        in states.(i) <- state
       ) !ns
    ) !map;
  {Simple.base = b; 
   Simple.dimension = d;
   Simple.states = states;
   Simple.name = states.(0).Simple.nameS;
   Simple.natural = set; 
   Simple.minimal = false;
   Simple.size = Array.length states;
   Simple.alphabet = abd}

  (** Takes a transition function from letters to formula, its basis
and generate the correct formula for the letters which are Letter *) 
(* The formula is going to be a Or for each letter s to formula f.  
We check if the variable are equal to s mod b, and then if f is true 
when modified by as needed according to s*)
let trans b delta =
  let res = ref  [] in 
  (* aux takes a letter, if it is a real Letter, it generate the
    formula needed and add it to res*)
  let aux letter form =
    match letter with 
    |Letter.Letter letter ->    
      let form = elim Math.N form in
      let right = inv_residual b letter form in
      let left = Array.mapi (fun i xi ->  (mod_ (Fr i, xi, b))) (IntAr.to_ar letter) in
      let left =  and_list (Array.to_list left) in
      res := imply (left, right) :: !res
    | _ -> ()
  in
  Transition.iter aux delta;
  and_list !res


(* let rec random natural m d = *)
(*   let x = Fr(Random.int d) and y = Fr(Random.int d) and *)
(*       k = Random.int m and i = Random.int m in *)
(*   let s =if natural= Math.N then 2 else 3 in *)
(*   let r =  *)
(*     match Random.int s with *)
(*     | 0 -> Const i *)
(*     | 1 -> Var (i,y) *)
(*     | _ -> Minus (i,y) *)
(*   and c = match Random.int 4 with *)
(*     | 0 -> Eq | 1 -> Less | 2 -> More | _ -> Mod k *)
(*   in *)
(*   match Random.int 6 with *)
(*   | 0 -> imply (random natural m d, random natural  m d) *)
(*   | 1 -> true_ *)
(*   | 2 -> false_ *)
(*   | 3 -> set (set_random natural  m d, And) *)
(*   | 4 -> set (set_random natural  m d, Or) *)
(*   | _ -> comp (x,r,c) *)
(*   and  *)
(*     set_random natural  m d = *)
(*     let rec aux acc =  *)
(*       if (Random.int 2) = 0  *)
(*       then aux (Hset.add (random natural m d) acc) *)
(*       else acc *)
(*     in  *)
(*     aux Hset.empty  *)
             

let rec random natural m d size  =
  (* fprintf std_formatter "random formula %d %d %d@." m d size; *)
  let gen_var () = Fr(Random.int d) in
  let gen_int () = Random.int m in
  let rec aux size =
    (* fprintf std_formatter "aux %d@." size; *)
    if size <=1
    then
      let s = if natural= Math.N then 2 else 3 in
      let r = 
        match Random.int s with
        | 0 -> Const (gen_int ())
        | 1 -> Var ((gen_int ()),gen_var ())
        | _ -> Minus ((gen_int ()),gen_var ())
      and c = match Random.int 4 with
        | 0 -> Eq | 1 -> Less | 2 -> More | _ -> Mod (gen_int ()+1)
      in
      let f = comp (gen_var(), r, c) in
      if Random.int 2= 0 then f else not_ f
    else
      let l = Random.int (size/2) in
      let r = size -l -1 in
      let l = aux l and r = aux r in
      if Random.int 2=0 
      then and_list [l;r] 
      else or_list [l;r]
  in 
  aux size
