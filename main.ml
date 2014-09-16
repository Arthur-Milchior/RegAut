Random.self_init ();;

open Format
let ppf = std_formatter;;

let filename =
  if Array.length Sys.argv > 1
  then Sys.argv.(1)
  else  "input";;

let input = open_in filename;;

let filebuf = Lexing.from_channel input;;


(*let aut_string = Simple.to_string aut;;
print_string ("The automaton is :\n"^ aut_string ^"\n ____________________\n");;
 *)

let debug _ = ();;
let output = (open_out "output");;
let ppf_file = formatter_of_out_channel output;;

type return =
  | Error of (Automaton.automaton * Automaton.NotReg.t )
  | Ok of Formula.t
  | Dif of Formula.t

let check_N simple =
    match Automaton.construct_simple Automaton.Mod Formula.Existential simple with
    | Automaton.Formula f ->
       let f = Formula.elim simple.Simple.natural f in
       let generated = Formula.to_aut simple.Simple.base simple.Simple.dimension simple.Simple.natural f in
       if Simple.equal simple generated 
       then Ok f
       else Dif f
    | Automaton.Error (aut,n) -> Error (aut,n)

(* let check_Z simple = *)
  
(*   match Automaton.construct_simple Automaton.Mod Formula.Existential simple with *)
(*   | Automaton.Formula f -> *)
(*      let f = Formula.elim simple.Simple.natural f in *)
(*      let generated = Formula.to_aut simple.Simple.base simple.Simple.dimension simple.Simple.natural f in *)
(*      if Simple.equal simple generated  *)
(*      then Ok f *)
(*      else Dif f *)
(*   | Automaton.Error n -> Error n *)


let rec print_check aut = 
  try 
    if aut.Simple.natural = Math.N then
      let res = check_N aut in
      match res with 
      | Ok f->
         fprintf ppf "%s: OK: %a !@." aut.Simple.name Formula.printf f
      | Dif f ->
         fprintf ppf "%s: we generate %a !@." aut.Simple.name Formula.printf f
      | Error (aut,n) ->
         fprintf ppf "%a: Error %a@." Automaton.printf aut Automaton.NotReg.printf n
  with 
  | Automaton.E (e,aut')->
     Automaton.printf ppf aut';
     print_check aut

(** Generate from input *) 
let main_input () =
  let simples = Parser.auts Lexer.aut filebuf in
  List.iter
    (fun elt ->
     match elt with
     | Simple.Mes mes ->
        fprintf ppf "%s@." mes;
     | Simple.Aut aut -> 
        print_check aut
    ) simples;;


let random_formula ()= 
  let b = 2 and set = Math.N in
  let graph = (open_out_gen [Open_append] 384 "formula.dat") in
  let graph_file = formatter_of_out_channel graph in
  for d = 1 to 10 do
    for i = 1 to 10 do
      let f = Formula.random set i d (i+1) in
      fprintf ppf "@.i=%d, d=%d.@. Random formula is %a@." i d Formula.printf f;
      let f = Formula.simplify set f in
      fprintf ppf "simplfied in %a@." Formula.printf f;
      let simple = Formula.to_aut b d set f in
      let simple = Simple.correct simple in
      let aut = Automaton.of_simple simple in 
      (* fprintf ppf "@.its aut is %a@." Simple.printf simple; *)
      (* Simple.printf_dot ppf_file simple; *)
      let beg = Sys.time () in
      let res = Automaton.exists Automaton.Mod aut in
      let time =( Sys.time()) -. beg in
      fprintf graph_file "%d %d %f@." (Array.length simple.Simple.states) d time;
      match res with 
      | None -> ()
      | Some er ->
         fprintf ppf "We have error %a@. On@.%a@." Automaton.NotReg.printf er Automaton.printf aut
    done
  done;
  flush graph;
  close_out graph
;;

let affiche simple = 
  Simple.printf_dot ppf_file simple;
  ignore(Sys.command "dot -Tsvg output >test.svg" (* "eog test.svg&"*));
  flush output ;;


let random_aut b d size nat = 
  let f = ref Formula.false_ in
  let simple = ref Simple.automaton_dummy in
  while  !f = Formula.true_ || !f =  Formula.false_ do
    simple:= Simple.random b d size Math.N;
      let formula = 
        (* if nat = Math.N  *)
        (* then *)
          Automaton.construct_simple Automaton.Mod Formula.Existential !simple
        (* else  *)
        (*   Relative.construct Automaton.Less !simple *)
      in
      match formula with 
      | Automaton.Formula formula ->
         f:= Formula.remove_simplify nat formula
      | Automaton.Error _ ->()
  done;
  fprintf ppf "The random automaton is @.%a@. and its formula is @.%a@." Simple.printf !simple Formula.printf !f;
  affiche !simple

let mesure nb b d size nat =
  let f_t = ref 0 in
  let er = ref 0 in
  let time_random = ref 0. in
  let time_exists = ref 0. in
  let time_construct = ref 0. in
  let min = ref max_float in
  let max = ref 0. in
  let beg = ref 0. in
  let errors = Array.make 32 0 in
  let finish _ = 
    let take = (Sys.time ())  -. !beg in
    time_exists := !time_exists +. take ;
    if !min > take then min := take;
    if !max < take then max := take
  in
  for i = 1 to nb do
    time_random := !time_random -. (Sys.time ());
    let simple = Simple.random b d size nat in
    time_random := !time_random +. (Sys.time ());
    (* if nat = Math.N *)
    (* then *)
    (*   ( *)
    let aut = Automaton.of_simple simple in
    beg := Sys.time ();
    let res =  Automaton.exists Automaton.Mod aut in
    finish ();
    match res with
    | Some n ->
       let e = Automaton.NotReg.count n in
       errors.(e) <- errors.(e)+1;
       incr er
    | None ->
       time_construct := !time_construct -. (Sys.time ());
       let f = Automaton.construct  Formula.Existential aut in
       time_construct := !time_construct +. (Sys.time ());
       let formula= Formula.remove_simplify nat f in
       if formula == Formula.true_ || formula == Formula.false_
       then incr f_t
  (* )else  *)
  (*   (let aut = Relative.of_simple Automaton.Less simple in *)
  (*    time_aut := !time_aut -. (Sys.time ()); *)
  (*    finish (); *)
  (*    time_construct := !time_construct -. (Sys.time ()); *)
  (*    Relative.resume aut) *)
  done;
  let total = (Sys.time ()) -. !beg in
  (!f_t, !er, !time_exists, !time_construct, !time_random, total, errors, !min, !max);;

let test () = 
  let graph = (open_out_gen [Open_append] 384 "graph.dat") in
  let graph_file = formatter_of_out_channel graph in
  let sizes = [1;2; 3;5;10;20;25;50;100; 250;500;1000;5000] in
  (* let unity = Math.gen_list 1 10 in *)
  (* let time_ten = List.map (( * ) 10) in *)
  (* let dec = time_ten unity in *)
  (* let hundreds = time_ten dec in *)
  (* let thousand = time_ten hundreds in *)
  (* let sizes = unity @ dec @ hundreds @ thousand in *)
  for d= 2 to 15 do
    List.iter 
      (fun i ->
       let f_t, er, ex, co, a, t, errors, min, max = mesure 100 2 d i  Math.N in
       fprintf ppf "100 random automaton of size %d, base: 2, dimension %d takes %f seconds for the existence and %f for the construction, raise %d errors and create %d booleans(and %f second to generate the automaton, in total %f, minimal time is :%f and maximal is :%f)" i d ex co er f_t a t min max;
       Pretty.print_array ppf "The errors are :@.@[[" ",@," "]@]@." errors (fun ppf i n -> fprintf ppf "(%d,%d)" i n) ;
       fprintf graph_file "%d %d %f@." i d ex
      ) sizes;
    flush graph
  done;
  ignore(Sys.command "./plot.pg; eog graph.png&");
  close_out graph
;;

open Formula 
let formula_bug ()= 
  let d = 3 and b = 2  in
  let vars = Formula.free d in
  let f =
    or_list [
        mode (vars.(0), 2, vars.(2),5);
        lesseq (vars.(1), vars.(2));
        lesseq (vars.(0), vars.(1));
        mode (vars.(2), 0, vars.(0), 2);
        const (vars.(1), 7);
      ]
  in
  let simple = Formula.to_aut b d Math.N f in
  let aut = Automaton.of_simple simple in
  ( match Automaton.exists Automaton.Mod aut  with
    | Some n -> 
       fprintf ppf "@.Error %a@.@.%a@." Automaton.NotReg.printf n Automaton.printf aut
    | None -> fprintf ppf "@.ok@."
  );
  affiche (Simple.minimize (Simple.two_to_four simple))
(* affiche (Automaton.to_simple aut) *)
;;

(* formula_bug () *)
  (* test() ;; *)

  (* main_input ();; *)

  (* random_aut 2 3 2 Math.N;; *)

random_formula ();;
