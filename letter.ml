open Format
type t = Letter of IntAr.t
       | ParenT of (Subset.t * Subset.t) 
       | ParenD of Subset.t 
       | ParenG of Subset.t  
       | ParenI of Subset.t 
       | CrochetT of (Subset.t * Subset.t) 
       | CrochetD of Subset.t 
       | CrochetI of (Subset.t * Subset.t)
       | ChevronT of (Subset.t * Subset.t) 
       | ChevronD of Subset.t 


type s = t
type word = t list

	 

let zero  =
  (fun b d -> Letter (IntAr.zero b d))


let printf ppf = function 
  | Letter ints -> IntAr.printf ppf ints
  | ParenD subset -> 
     fprintf ppf "@[(%a)@]" Subset.printf  subset
  | ParenG subset -> 
     fprintf ppf "@[(∅,%a)@]" Subset.printf  subset
  | ParenT (subset , subset2) ->
     fprintf ppf "@[(%a@,,%a)@]"
             Subset.printf subset
             Subset.printf  subset2
  | CrochetD subset ->
     fprintf ppf "@[[%a]@]" Subset.printf subset
  | CrochetT (subset , subset2) ->
     fprintf ppf "@[[%a@,,%a]@]"
     Subset.printf subset
     Subset.printf  subset2
  | ChevronD subset ->
     fprintf ppf "@[ <%a>@]"
     Subset.printf  subset
  | ChevronT (subset , subset2) ->
     fprintf ppf "@[ <%a,%a>@]"
     Subset.printf  subset
     Subset.printf  subset2
  | ParenI subset ->
     fprintf ppf "@[(%a)^{-1}@]"
     Subset.printf  subset
  | CrochetI (subset , subset2) ->
     fprintf ppf "@[[%a@,,%a]^{-1}@]"
     Subset.printf  subset
     Subset.printf  subset2

let check_dim b d = function
  | Letter ints -> IntAr.check_dim b d ints
  | ParenI subset  | ParenG  subset    | ChevronD  subset   | CrochetD  subset   | ParenD  subset -> Subset.check_dim d subset
  | ParenT (subset , subset2) 
  | CrochetT (subset , subset2) 
  | ChevronT (subset , subset2)  
  | CrochetI (subset , subset2) ->
     Subset.check_dim d subset && Subset.check_dim d subset2

let int_to_word ar b =
  let l = ref [] in
  let d = Array.length ar in
  (* cloned because we will modifiate it *)
  let ar= Array.copy ar in
  let loop =  ref true
  in 
  while !loop  do
    loop := false;
    let letter = Array.make d 0 in    
    Array.iteri
      (fun i v ->
       if v>0 then loop := true;
       letter.(i) <- v mod b;
       ar.(i)<- v/b;
      ) ar;
    if !loop  then  l:= (Letter (IntAr.make b d letter)) :: !l;
  done;
  !l
module Comp =
  struct
    type t = s
    let compare x y = 
      match x, y with
      | Letter x, Letter y -> IntAr.compare x y
      | Letter _, _ -> 1
      | _, Letter _ -> -1

      | ParenT (x,a), ParenT (y,b) -> 
         let c = Subset.compare x y in
         if c = 0 
         then Subset.compare a b
         else c
      | ParenT _, _ -> 1
      | _, ParenT _ -> -1

      | ParenD x, ParenD y -> Subset.compare x y
      | ParenD _, _ -> 1
      | _, ParenD _ -> -1

      | ParenG x, ParenG y -> Subset.compare x y
      | ParenG _, _ -> 1
      | _, ParenG _ -> -1

      | ParenI x, ParenI y -> Subset.compare x y
      | ParenI _, _ -> 1
      | _, ParenI _ -> -1

      | CrochetT (x,a), CrochetT (y,b) -> 
         let c = Subset.compare x y in
         if c = 0 
         then Subset.compare a b
         else c
      | CrochetT _, _ -> 1
      | _, CrochetT _ -> -1

      | CrochetD x, CrochetD y -> Subset.compare x y
      | CrochetD _, _ -> 1
      | _, CrochetD _ -> -1

      | CrochetI (x,a), CrochetI (y,b) -> 
         let c = Subset.compare x y in
         if c = 0 
         then Subset.compare a b
         else c
      | CrochetI _, _ -> 1
      | _, CrochetI _ -> -1

      | ChevronT (x,a), ChevronT (y,b) -> 
         let c = Subset.compare x y in
         if c = 0 
         then Subset.compare a b
         else c
      | ChevronT _, _ -> 1
      | _, ChevronT _ -> -1

      | ChevronD x, ChevronD y -> Subset.compare x y
  end

module M = Map.Make (Comp)

let double l = 
  match l with 
  | ParenG (b) -> let d = Subset.dimension b in
                  let u = Subset.empty d in
                  ParenT(u,b)
  | ParenD (u) -> let d = Subset.dimension u in
                  let b = Subset.empty d in
                  ParenT(u,b)
  | CrochetD (u) ->let d = Subset.dimension u in
                   let b = Subset.empty d in
                   CrochetT(u,b)
  | ChevronD (u)  -> let d = Subset.dimension u in
                     let b = Subset.empty d in
                     ChevronT(u,b)
  | ParenI _ | CrochetI _ -> raise (Invalid_argument "Double on inverse letter")
  | letter -> letter
