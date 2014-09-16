open Format
module MM = IntAr.M
type state = {
  delta : int MM.t;
  id: int ;
  final : bool ;
  nameS : string ; 
} 
type automaton = { base : int;
                   dimension : int ;
                   mutable states : state array ;
                   name: string;
                   natural : Math.set;
                   alphabet : IntAr.t list;
                   size : int;
                   minimal : bool;
                 }

type input = Aut of automaton | Mes of string
  
let state_dummy =
  {delta = MM.empty ; id = -1;  final = false; nameS = "dummy"}
let automaton_dummy
  ={base = 0; dimension = 0; states = [|state_dummy|]; name =""; natural = Math.N; alphabet = []; size= -1; minimal = false}

let check aut =
  let size = aut.size in
  Array.fold_left
    (fun bool state->
     MM.fold
       (fun letter state b ->
        b &&  aut.dimension = (IntAr.dimension letter) && state<size
       )  state.delta bool
    ) true  aut.states 

(** take an automaton, return a tab such that tab.(i).(j) is the list of letter from i to j *)
let to_dot aut =
  let size = aut.size in
  let tab = Array.make_matrix size size [] in
  Array.iter 
    (fun state->
     MM.iter
       (fun letter suc->
        tab.(state.id).(suc) <- letter :: tab.(state.id).(suc))
       state.delta
    ) aut.states;
  tab
  
let print_state ppf state =
  fprintf ppf "%s \"%s\" id=%d, transition: " (if state.final then "accepting" else "rejecting") state.nameS state.id ;
  Pretty.print_list ppf "`" "," "'" (MM.bindings state.delta) (fun ppf (ints, suc) -> fprintf ppf "%a -> %d" IntAr.printf ints suc)

let printf ppf aut =
  fprintf ppf "{ automaton %s of dim:%d, base:%d is %s@." aut.name aut.dimension aut.base (if aut.natural= Math.N then "natural" else "relative");
  Pretty.print_array ppf "<" ",@,"">}" aut.states (fun ppf _ state -> print_state ppf state)

let state_id ppf state =
  fprintf ppf "%d" state.id

let state_name ppf state =
  fprintf ppf "%s" state.nameS

let printf_dot ppf aut =
  let print_state = state_name in
  let dots = to_dot aut in
  fprintf ppf "digraph finite_state_machine@ {@[rankdir=LR;@.";
  Pretty.print_array ppf "node [shape = doublecircle];@ @[" "@," "@]" aut.states
                     (fun ppf i s ->
                      if s.final
                      then fprintf ppf "\"%a\";@," print_state aut.states.(i) (* fprintf ppf "q%d;@," i *)
                     );
  Pretty.print_array ppf "node [shape = circle];@ @[" "@," "@]" aut.states
                     (fun ppf i s ->
                      if not s.final
                      then fprintf ppf "\"%a\";@," print_state aut.states.(i)(* fprintf ppf "q%i;@," i *)
                     );
  Pretty.print_array ppf "" "" "" dots (* print the array *)
                     (fun ppf i line-> 
                      Pretty.print_array ppf "" "" "" dots (* print the line *)
                                         (fun ppf j elt -> 
                                          if dots.(i).(j) <> [] 
                                          then
                                            (fprintf ppf "@.\"%a\" -> \"%a\"" print_state  aut.states.(i) print_state  aut.states.(j) ;
                                             Pretty.print_list ppf "[label = \"" "," "\"]" (* print the transition list *)
                                                               dots.(i).(j)
                                                                          (fun ppf letter->
                                                                           fprintf ppf "%a" IntAr.printf letter
                                                                          )
                                            )
                                         )
                     );
  fprintf ppf "}@]@."


let two_to_four aut =
  if aut.base > 3
  then aut 
  else let size = aut.size 
       and d = aut.dimension in
       let base = aut.base * aut.base in
       let abd = IntAr.enum base d in
       let aut2 = { aut with
                    base = base;
                    states = Array.make size state_dummy;
                    alphabet= abd;
                  }  in
       for i = 0 to size -1 do
         let trans =
           MM.fold
             (fun letter1 inter_state trans ->
              MM.fold
                (fun letter2 suc trans ->
                 let letter = IntAr.concat letter1 letter2 in
                 MM.add letter suc trans
                ) aut.states.(inter_state).delta  trans
             )  aut.states.(i).delta MM.empty
         in 
         aut2.states.(i) <- {aut.states.(i) with delta= trans; id = i}
       done;
       aut2


let minimize aut = 
  if aut.minimal 
  then aut 
  else
    let abd = aut.alphabet in
    let states = aut.states in
    let size = aut.size in
    let min = ref (Array.make size size) in (* at step s, min.(i) is the least element equivalent to i in the sth approximation *)
    let min' = ref (Array.make size size) in (* at step s, min.(i) is the least element equivalent to i at the s+1th approximation*)
    let final = ref None (*least accepting position*)
    and reject = ref None in (*least rejecting position*)
    for i=0 to size-1 do
      match states.(i).final, !final, !reject with
      | true, None, _ -> (!min).(i) <- i; final := Some i 
      | true, Some m, _ -> (!min).(i) <- m
      | false, _, None -> (!min).(i) <- i; reject := Some i
      | false, _, Some m -> (!min).(i) <- m
    done;
    (* now, min correspond to step 0, there is at most two class, accepting and rejecting *)
    let change = ref true in
    while !change do
      (* Pretty.print_array std_formatter "min is [""|""]@." !min (fun ppf i suc -> fprintf ppf "%d->%d" i suc); *)
      change:= false;
      for state=0 to size-1 do
        (!min').(state)<- state;
        for state'=state-1 downto 0 do
          let is_equiv =  (!min).(state) = (!min).(state') && (*is it still equal *)
                            List.fold_left 
                              (fun equiv letter ->
                               let suc_state = (*the successor of state by letter *)
                                 try
                                   ((!min).(MM.find letter states.(state).delta))
                                 with | Not_found-> fprintf std_formatter "erreur sur l'automate %a" printf aut;
                                                    raise Not_found
                               in               
                               let suc_eq_state = (*the successor of min_eq_state by letter *)
                                 ((!min).(MM.find letter states.(state').delta))
                               in 
                               if suc_state = suc_eq_state
                               then equiv
                               else false
                              ) true abd
          in if is_equiv then (!min').(state)<- state';
        done; 
        if (!min').(state)<> (!min).(state)
        then change := true 
      done;
      min := !min';
      min':= Array.make size (-1); 
    (* Pretty.print_array std_formatter "and become [""|""]@." !min (fun ppf i suc -> fprintf ppf "%d->%d" i suc); *)
    (* fprintf std_formatter "so it has %s changed@." (if !change then "" else "not") *)
    done;
    (* Pretty.print_array std_formatter "finaly min is [""|""]@." !min (fun ppf i suc -> fprintf ppf "%d->%d" i suc); *)
    (*now min sends each state to its minimal equivalent state *)
    let new_number = Array.make size (-1) in
    let remove = ref 0 in 
    for i = 0 to size -1 do
      if (!min).(i) = i  
      then
        new_number.(i) <- i- !remove
      else incr remove
    done;
    (* Pretty.print_array std_formatter "new_number is [""|""]@." !min (fun ppf i suc -> fprintf ppf "%d->%d" i suc); *)
    let size' =  size - !remove in
    let states' = Array.make size' state_dummy in
    for i = 0 to size -1 do
      if (!min).(i) = i
      then 
        let delta = 
          MM.map
            (fun suc ->
             new_number.((!min).(suc))
            ) states.(i).delta in
        let new_id = new_number.(i) in
        let name = ref states.(i).nameS in
        for j=i+1 to size-1 do
          if (!min).(j)= i 
          then
            name := !name ^  "| " ^ states.(j).nameS
        done ;
        let state' = {states.(i) with delta = delta; id = new_id; nameS = !name } in
        states'.(new_id) <- state'
    done;
    {aut with  states=states'; size = Array.length states'; minimal = true} 

(** return the position of the garbage; or None
  and a boolean stating if we need to add a garbage *)
let find_garbage aut = 
  let bd = Math.power aut.base aut.dimension in
  let value = ref None in
  let need = ref false in
  Array.iter
    (fun state->
     let found = ref (not state.final) in
     let size = MM.fold
                  (fun _ suc i ->
                   if state.id != suc 
                   then found := false;
                   i+1
                  ) state.delta 0 in
     if size <> bd then need:= true;
     if !found 
     then value := Some state.id
    ) aut.states;
  (!value, !need)


let add_garb aut =
  let value, need = find_garbage aut in
  if not need 
  then aut
  else 
    let size = aut.size in
    let size', garbage =
      match value with
      | Some i -> size, i
      | None -> size+1, size 
    in
    let states = aut.states in
    let abd = aut.alphabet in
    let states' = Array.make size' state_dummy in
    for i = 0 to size-1 do
      let trans = 
        List.fold_left
          (fun trans letter ->
           let suc =
             try MM.find letter states.(i).delta 
             with |Not_found -> garbage
           in MM.add letter suc trans
          )MM.empty abd 
      in
      states'.(i) <- {states.(i) with delta = trans};
    done;
    if size'= size +1 
    then
      states'.(size) <-{id= size; 
                        delta = List.fold_left (fun acc l -> MM.add l size acc) MM.empty abd;
                        final = false;
                        nameS = "garbage"};
    {aut with states=states'; size = size'; }

(**keep the accessible state *)
let accessible aut =
  let states = aut.states in
  let size = aut.size in
  let accessible = Array.make size false in
  let rec aux state =
    if not accessible.(state) then
      (accessible.(state)<- true;
       MM.iter (fun _ suc-> aux suc) states.(state).delta)
  in
  aux 0;
  let new_number = Array.make size (-1) in
  let remove = ref 0 in 
  for i = 0 to size -1 do
    if accessible.(i) then
      new_number.(i) <- i- !remove
    else incr remove
  done;
  let size' =  size - !remove in
  let states' =  Array.make size' state_dummy in
  for i = 0 to size -1 do
    if accessible.(i) 
    then 
      let delta = 
        MM.map (fun suc -> new_number.(suc)) states.(i).delta in
      let new_id = new_number.(i) in
      let state' = {states.(i) with delta = delta; id = new_id; final = states.(i).final} in
      states'.(new_id) <- state'
  done;
  {aut with  states=states';size = size';} 


  (* state (q,i) will be 2q+1-i *)
let zToN aut =
  let aut = two_to_four aut in
  let aut = add_garb aut in
  let d = aut.dimension in
  let b = aut.base in
  let size = aut.size in
  let zero = IntAr.zero  b d in
  let size' = size *2 in
  List.map
    (fun n ->
     let aut2 ={aut with states = Array.make size' state_dummy; size = size'; minimal = false} in
     for i = 0 to size -1 do
       let trans = ref MM.empty
       and suc_final = ref false in
       let state = aut.states.(i) in
       MM.iter
         (fun letter suc ->
          let letter' = IntAr.inv letter n in
          let is_zero = letter' = zero in
          let suc' = 2* suc +(if is_zero then 1 else 0) in
          trans :=  MM.add letter' suc' !trans ;
          if is_zero then suc_final := aut.states.(suc).final;
         ) state.delta;
       aut2.states.(2*i) <- {delta= !trans; id =2*i; final = !suc_final; nameS = state.nameS ^ "1"};
       aut2.states.(2*i+1) <- {delta= !trans; id = 2*i+1; final = state.final; nameS = state.nameS ^ "0"} ;
     done;
     n, minimize (accessible aut2)
    )(Subset.enumerate d)
  
  
let correct aut = 
  let aut' = add_garb aut in
  let aut' = two_to_four aut' in
  let aut' = minimize aut' in
  let aut' = accessible aut' in
  aut'

let remove_0 aut=
  (* fprintf std_formatter "Applying remove_0 to %a@.@." printf aut; *)
  let d= aut.dimension in
  let size = aut.size in
  let res = Array.make d automaton_dummy in
  for i = 0 to d-1 do
    let aut_i = {aut with dimension = d-1; states = Array.make size state_dummy; minimal=false} in
    res.(i) <- aut_i ;
    for j =0 to size -1 do
      let state = aut.states.(j) in
      let delta =
        MM.fold 
          (fun letter suc acc->
           if IntAr.get letter i = 0 then
             let letter' = IntAr.remove i letter in
             MM.add letter' suc acc
           else acc
          )  state.delta MM.empty in
      aut_i.states.(j) <- {state with delta = delta}
    done
  done;
  (* fprintf std_formatter "we obtain before minimization:@."; *)
  (* Pretty.print_array std_formatter "@[[(" "),@,(" ")]@]" res (fun ppf i sim-> fprintf ppf "%d,%a" i printf sim); *)
  let res = Array.map correct res in
  (* fprintf std_formatter "remove 0 on %a@. gives:@." printf aut; *)
  (* Pretty.print_array std_formatter "@[[(" "),@,(" ")]@]@." res (fun ppf i sim-> fprintf ppf "%d,%a" i printf sim); *)
  res

let remove_1 aut = 
  let d = aut.dimension in
  let b = aut.base in
  let abd = aut.alphabet in
  let subsets = Subset.enumerate d in
  let twoToD =  1 lsl d in
  let size = aut.size in
  let size' = twoToD * size in
  let states'= Array.make size' state_dummy in
  let aut' = {aut with states= states' ; minimal=false} in
  for i=0 to size-1 do
    let state = aut.states.(i) in
    List.iter 
      (fun retenu ->
       (* we use (twoToD -1- (Subset.value retenu)) because we must have 0 for the total set *)
       let id'= twoToD * state.id + (twoToD - (Subset.value retenu)-1) in
       let access_real = IntAr.of_par b d retenu in
       let real_state = MM.find access_real state.delta in
       let final = aut.states.(real_state).final in
       let delta = 
         List.fold_left 
           (fun acc letter ->
            let letter',retenu' = IntAr.incr letter retenu in
            let suc' = MM.find letter' state.delta in
            let suc' = twoToD * suc' + twoToD - (Subset.value retenu')-1
            in MM.add letter suc' acc
           ) MM.empty abd
       in
       states'.(id')<-{aut.states.(i) with delta = delta; final = final; id = id'}
      ) subsets
  done;
  (* fprintf std_formatter "we obtain before minimization %a@.@." printf aut'; *)
  let aut'= correct aut' in
  (* fprintf std_formatter "and after minimization %a@.@." printf aut'; *)
  (* fprintf std_formatter "remove 1 on@, %a@. gives %a@." printf aut printf aut'; *)
  aut'


let equal aut1 aut2 = 
  let d = aut1.dimension in
  let b = aut1.base in
  if b= aut2.base && d= aut2.dimension 
  then
    (let abd = aut1.alphabet in
     let aut1 = correct aut1 
     and aut2 = correct aut2 in
     let map = ref IntMap.empty in
     let rec aux q1 q2 =
       if IntMap.mem q1 !map 
       then q2 = IntMap.find q1 !map
       else 
         (map := IntMap.add q1 q2 !map ;
          List.fold_left 
            (fun eq a->
             let q1 = MM.find a aut1.states.(q1).delta
             and q2 = MM.find a aut2.states.(q2).delta in
             let eq' = aux q1 q2 in
             eq && eq' 
            ) true abd
         )
     in aux 0 0;
    )
  else false

let id =  ref 0

let random b d size set =
  let states = Array.make size state_dummy in
  let abd = IntAr.enum b d in
  for i = 0 to size-1 do
    let final = (Random.int 2)=0 in
    let delta = 
      List.fold_left 
        (fun delta a ->
         MM.add a (Random.int size) delta
        ) MM.empty abd
    in
    let name = (string_of_int !id) ^"-"^(string_of_int i)in 
    states.(i)<- {id = i; delta = delta; final = final; nameS = name}
  done;
  let name = ("Random "^ (string_of_int !id))in
  incr id;
  {states = states; name =name; natural = set; alphabet= abd; dimension=d; base= b; size = size; minimal=false}
