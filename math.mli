exception Found
(** the gcd; O(log ) *)
val gcd : int -> int -> int
(** the lcm; O(log )*)
val lcm : int -> int -> int
(** the lcm of a list of integer; O(size * log(sum elements) ) *)
val lcms : int list -> int
(** gen_list a b is [a..b] O(n)*)
val gen_list : int ->int ->int list
(** remove the Sure, fail if its a None O(1)*)
val sure : 'a option -> 'a
(** boolean xor O(1)*)
val xor : bool -> bool -> bool
(** power b d  is b^d as integer (O(log d)*)
val power : int ->int ->int
(** mul_inv a d return the inverse of a modulo d O(log)*)
val mul_inv : int ->int ->int
(** mod a b return the least positive value equiv to a mod d O(1)*)
val mod_ : int ->int ->int
(** the kind of set we may use *)
type set = N|Z
(** the logarithm in base 2 of an int, as an int O(1)*)
val log : int ->int
(** the division, floored even for negative value O(1)*)
val div_floor :  int ->int ->int
(** the division, ceiled even for positive value O(1)*)
val div_ceil :  int ->int ->int
(** the division by 2, floored even for negative value O(1)*)
val half_floor :  int ->int
(** the halfision by 2, ceiled even for positive value O(1)*)
val half_ceil :  int  ->int

module M: Map.S with type key = int

(** the list of pairs O(n ^2) *)
val list_pair : 'a list -> ('a * 'a) list

(** option_map f y apply f to x if it's Some x; O(f)*)
val option_map : ('a -> 'b) -> 'a option -> 'b option


(** The value of the maximal dimension *)
val max_dim : int

(** discrete_log b d return the list e such that b^e=1 mod d*)
val discrete_log: int -> int -> int
