exception N of (Automaton.NotReg.t * Automaton.automaton * Subset.t)
exception R of (Automaton.NotReg.t)

type t
val construct : Automaton.search -> Simple.automaton ->  Formula.t
val of_simple :Automaton.search -> Simple.automaton -> t
val resume : t -> Formula.t
