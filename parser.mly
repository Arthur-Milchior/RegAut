%{ 
  let id = ref (- 1);;
  type number = Number of int  | Dot;;
%}
%token <int> INT
%token <string> STRING
%token OA CA OB CB OP CP ARROW FINAL COMMA PIPE EOF DOT NATURAL RELATIVE

%type <(number list list * int)> transition 
%type <bool * ((number list * int) list) * int* string> state
%type <Simple.automaton> aut
%type <string> string
%type <Simple.input> input
%type <number> int
%type <number list> letter
%start <Simple.input list> auts
%type <Math.set> nat
%%

transition:
| letter=separated_list(option(COMMA),letter) ARROW  suc =INT {letter, suc}

letter:
|OB letter = separated_list(sep,int)  CB {letter}

state:
| OP s=option(string) option(COMMA) f=boption(final) trans = separated_list(option(COMMA),transition) CP 
        {incr id;
         let s = match s with | None -> ("q"^(string_of_int !id)) |Some s -> s in
         let trans = List.concat (List.map 
           (fun (letters, suc ) ->
            List.map (fun letter -> (letter,suc)) letters
           ) trans) in
         (f,trans, !id,s)}

sep:
|COMMA {}
|PIPE {}

aut:
| OA s=option(string) option(COMMA) n=option(nat) option(COMMA) b=INT d = INT states=list(state) CA 
       {id:= -1;
        let nat = match n with |None -> Math.N | Some n -> n in
        let zero_b = Math.gen_list 0 (b-1) in
        let states = match states with 
          | [] -> [false, [], 0, ""]
          | _ -> states
        in
        let states = List.map
          (fun (f, trans, id, name) ->
           let transition =
             List.fold_left 
               (fun transition (ints, suc)->
                (* takes the firsts element of the transition, 
                   a list of integer and resume *)
                let rec aux transition constructed = function
                  | [] -> 
                     let letter = (Array.of_list (List.rev constructed))  in
                     let letter = IntAr.make b d letter 
                     in IntAr.M.add letter suc transition
                  | Number i :: t ->
                     aux transition (i:: constructed) t
                  | Dot:: t -> 
                     List.fold_left
                       (fun transition i->
                        aux transition (i:: constructed) t)
                       transition zero_b
                in aux transition [] ints
               ) IntAr.M.empty trans
           in {Simple.delta = transition; Simple.id = id; Simple.final=f; Simple.nameS= name}
          ) states
        in
        {Simple.name = (match s with None -> ""|Some s -> s);
         Simple.base =b;
         Simple.dimension=d;
         Simple.minimal= false ;
         Simple.states=
           if states = [] 
           then [|Simple.state_dummy|]
           else Array.of_list states;
         Simple.natural = nat;
         Simple.size = List.length states;
         Simple.alphabet = IntAr.enum b d }}

int:
| i = INT {Number i}
| DOT {Dot}
input:
| s= string {Simple.Mes s}
| a= aut {Simple.Aut a}

string :
| s=STRING {s}
auts :
| l=list(input)  EOF {l}

final:
| FINAL option(COMMA) {}

nat:
| NATURAL {Math.N}
| RELATIVE {Math.Z}
