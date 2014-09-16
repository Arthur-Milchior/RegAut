open Format

module Comp = 
  struct 
    type t = {ar : int array; v : int; d: int; b:int}
    let compare x y = x.v - y.v
  end

module M = Map.Make(Comp)
module S = Set.Make(Comp)
include Comp

let make b d ar = 
  let bi = ref 1 in
  let v = ref 0 in
  for i = 0 to d-1 do
    v:= !v + ar.(i) * !bi;
    bi:= b * !bi
  done;
  {ar = ar; v = !v; b =b; d=d}

let enum_non_r b d =
  let arr = Array.make d 0 in
  let res = ref [] in
  let rec aux pos arr bi v=
    if pos = d
    then res := {ar =arr; v=v; b=b; d=d} :: !res
    else 
      for i=1 to b-2 do
	let arr = Array.copy arr in
	arr.(pos) <- i;
	aux (pos +1) arr (bi * b) (v+ bi * i)
      done
  in aux 0 arr 1 0;
     !res

let enum b d =
  let arr = Array.make d 0 in
  let res = ref [] in
  let rec aux pos arr bi v=
    if pos = d
    then res := {ar =arr; v=v; b=b; d=d} :: !res
    else 
      for i=0 to b-1 do
	let arr = Array.copy arr in
	arr.(pos) <- i;
	aux (pos +1) arr (bi * b) (v+ bi * i)
      done
  in aux 0 arr 1 0;
     !res

let printf ppf l =
  Pretty.print_array ppf "[" "|" "]" l.ar (fun ppf _ i -> fprintf ppf "%d" i)
  (* fprintf ppf ":%d:b=%d:d=%d" l.v l.b l.d *)

let zero =
  let map = Hashtbl.create Math.max_dim in
  fun b d->
  try Hashtbl.find map (b,d) 
  with | Not_found ->
          let t= {ar=Array.make d 0; v = 0; b=b; d=d} in
          Hashtbl.add map (b,d) t ;
          t

let check_dim b d { ar=ints; b=b'; d=d'} =
  Array.fold_left (fun acc i -> acc && i<b) 
                  (b=b' && d=d' && d = Array.length ints) 
                  ints

let get {ar = a} i= a.(i)

let of_par2 b d s1 s2 =
  let letter = Array.make d 0 in 
  for i =0 to (d -1 ) do
    if Subset.ins s2 i then letter.(i) <- b -1;
    if Subset.ins s1 i then letter.(i) <- 1;
  done;
  make b d letter

let of_par b d s1 =
  let letter = Array.make d 0 in 
  for i =0 to (d -1 ) do
    if Subset.ins s1 i then letter.(i) <- 1;
  done;
  make b d letter
 
let copy t = {t with ar = Array.copy t.ar}

let set t i v =
  let bi = Math.power t.b i in
  let ar = Array.copy t.ar in
  let dif = v- ar.(i) in
  ar.(i)<- v;
  let v = t.v + (dif * bi ) in
  {t with ar = ar; v= v}

let decr t s =
  let ar = Array.copy t.ar in
  let v = ref t.v 
  and bi = ref 1 in
  for i = 0 to t.d -1 do
    if Subset.ins s i 
    then 
      (ar.(i) <- ar.(i) -1;
       v := !v - !bi);
    bi := !bi * t.b
  done;
  {t with  ar = ar; v = !v}

let incr t s =
  let d = t.d and b = t.b in  
  let ar = Array.copy t.ar in
  let v = ref t.v 
  and bi = ref 1 in
  let retenu = ref (Subset.empty d) in
  for i = 0 to d -1 do
    if Subset.ins s i 
    then 
      (ar.(i) <- (ar.(i) +1) mod b;
       if ar.(i) = 0 
       then 
         (retenu := Subset.add !retenu i;
          v:= !v - (b-1) * !bi
         )
       else
         v := !v + !bi);
    bi := !bi * b
  done;
  ({t with  ar = ar; v = !v}, !retenu )

let to_ar {ar = a} = a

let dimension {d=d}=d

let concat  letter1 letter2  =
  let d= letter1.d in
  let letter = Array.make d 0 in
  for i = 0 to d -1 do
    letter.(i) <- letter1.ar.(i) + letter1.b *letter2.ar.(i)
  done ;
  make (letter1.b * letter2.b) d letter

let inv t n =
  let d = t.d and b = t.b in  
  let letter' = Array.make d 0 in
  for i = 0 to d -1 do
    letter'.(i) <-
      if Subset.ins n i
      then b-1- t.ar.(i)
      else t.ar.(i)
  done ;
  make b d letter'


let remove i t=
  let d = t.d and b = t.b in  
  let letter' =  Array.make (d-1) 0 in
  for k = 0 to i-1 do
    letter'.(k) <- t.ar.(k)
  done; 
  for k = i to d-2 do
    letter'.(k) <- t.ar.(k+1)
  done; 
  make b (d-1) letter'
