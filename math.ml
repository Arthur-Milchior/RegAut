exception Found
(** A file containing some math function used in this program *)
let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

(** the lcm of a list of element *)
let lcms l=
  List.fold_left lcm 1 l

let rec gen_list first last =
  if first> last then 
    []
  else first :: gen_list (first+1) last

let sure = function
  | Some x -> x
  | None -> raise (Failure "None")

let xor = (<> )

let rec power b d =
  if d =0 
  then 1
  else if d mod 2 = 1 
  then b * (power b (d-1))
  else power (b*b) (d/2)

let mul_inv a = function 1 -> 1 | b ->
  let rec aux a b x0 x1 =
    if a <= 1 then x1 else
    if b = 0 then failwith "mul_inv" else
    aux b (a mod b) (x1 - (a / b) * x0) x0
  in
  let x = aux a b 0 1 in
  if x < 0 then x + b else x

let mod_ a b =
  let r = a mod b in
  if r<0 then r+b else r

type set = N | Z

let log2 = log 2.

let log x = int_of_float (floor ((log (float x))/. log2))

(** half of the value, floored *)
let half_floor x = 
  let x = x - (x mod 2) in
  x/2 

(** half of the value, ceiled *)
let half_ceil x = 
  let x = x + (x mod 2) in
  x/2 

(** half of the value, floored *)
let div_floor a b = 
  let a = a - (mod_ a b) in
  a/b 

(** half of the value, ceiled *)
let div_ceil a b = 
  let a = a + (mod_ (b-a) b) in
  a/b

module Comp= 
  struct
    type t = int
    let compare = (-)
  end

module M = Map.Make (Comp)

let list_pair l=
  let rec aux acc =function
    | [] ->[]
    | h::t ->
       let acc = List.fold_left
                   (fun acc e -> (h,t)::acc) acc t
       in aux acc t
  in aux [] l

let option_map f = function
  | None -> None
  | Some a -> Some (f a)

let max_dim = 30

let discrete_log b d =
  let i = ref 1 in
  let m = ref (b mod d) in
  while !m>1 do
    incr i;
    m:= (!m*b) mod d
  done;
  !i
