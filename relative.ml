open Format;;
exception N of (Automaton.NotReg.t * Automaton.automaton * Subset.t)
exception R of (Automaton.NotReg.t)

type t = int * int * (Subset.t * Automaton.automaton) list

let of_simple set simple =
  let b = simple.Simple.base in
  let d = simple.Simple.dimension in
  if simple.Simple.states.(0).Simple.final 
  then raise (R Automaton.NotReg.Prop_rel_init);
  List.iter
    (fun a->
     Array.iter 
       (fun s->
        let suc = IntAr.M.find a s.Simple.delta in
        if simple.Simple.states.(suc).Simple.final 
        then raise (R (Automaton.NotReg.Prop_rel (s.Simple.id,a)));
       )simple.Simple.states
    )(IntAr.enum_non_r b d) ;
  let simples = Simple.zToN simple in
  let auts = 
    List.map
      (fun (n,simple) -> 
       try
         let aut = Automaton.of_simple set simple in 
         (* Automaton.checkN n aut; *)
         (n,aut)
       with 
       | Automaton.N (x, a) -> raise (N(x,a,n))
      ) simples in
  (b,d,auts)

let resume (b,d,auts) =
  let formulas = List.map (fun (n,aut) -> (n,Formula.simplify_natural (Automaton.construct Formula.Existential aut))) auts in
  let x = Formula.free d in
  let knowns = ref [] in
  for i = 0 to d-1 do
    let cond = Formula.neg (x.(i)) in
    let known = Formula.or_list [cond; Formula.not_ cond] in
    let known = Formula.assert_ known in
    knowns := known :: !knowns ;
  done;
  let knowns = Formula.and_list !knowns in
  let formulas =
    List.map 
      (fun (n,f) ->
       let l =List.map 
                (fun i->
                 let cond =  Formula.neg (x.(i)) in
                 if Subset.ins n i
                 then cond
                 else Formula.not_ (cond)
                ) (Math.gen_list 0 (d-1)) in
       (* fprintf std_formatter "change formula from %a" Formula.printf f; *)
       let r = Formula.change_n n f in
       (* fprintf std_formatter "to %a@." Formula.printf r; *)
       let l = Formula.and_list l in
       Formula.imply (l,r)
      ) formulas
  in Formula.and_list (knowns:: formulas)

let construct set simple = 
  let t = of_simple set simple in
  resume t
