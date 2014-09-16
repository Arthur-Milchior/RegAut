open Format
let print_list ppf left sep right list f=
  fprintf ppf left;
  List.iteri (
      fun i elt ->
      if i>0
      then fprintf ppf  sep;
      f ppf elt
    ) list;
  fprintf ppf right

let print_set ppf left sep right list f=
  fprintf ppf left;
  let first = ref true in
  Hset.iter (
      fun elt ->
      if not !first
      then fprintf ppf  sep
      else first := false ;
      f ppf elt
    ) list;
  fprintf ppf right

(** Print an array, on a ppf,  with left border, seperator, right border, and a function that takes the ppf, the position and the element *)
let print_array ppf left sep right array f=
 fprintf ppf left;
  Array.iteri (
      fun i elt ->
      if i>0
      then fprintf ppf  sep;
      f ppf i elt
    ) array;
  fprintf ppf right


(** print None for  None, and print the element without Some if its Some *)

let some f ppf  = function 
         | None -> fprintf ppf "None" 
         | Some x -> f ppf x

let string printer value =
  let buf = Buffer.create 50 in
  let form = Format.formatter_of_buffer buf in
  fprintf form "%a@?" printer value;
  Buffer.contents buf
