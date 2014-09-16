open Format
type 'state  t = 'state Letter.M.t ;;

let successor  = Letter.M.find

(** Add an transition from a letter to a state *)
let add delta letter state =
   Letter.M.add letter state delta

let empty =Letter.M.empty;;

let iter f delta =
  Letter.M.iter f delta

let fold f init delta =
  Letter.M.fold f delta init

let map f delta =
  Letter.M.map f delta

(* let from_list l = (Letter.M.bindings l) *)
let printf f ppf delta =
  Pretty.print_list ppf  "@[{" "," "}@]"
                    (Letter.M.bindings delta)
                    (fun ppf (l, s) ->
                     Letter.printf ppf l;
                     fprintf ppf "->@ ";
                     f ppf s
                    ) 


