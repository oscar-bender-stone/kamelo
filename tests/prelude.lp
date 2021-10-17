
// Module BASIC-K
symbol SortK : TYPE;
symbol SortKItem : SortK;

// Module INJ
// δ ζ θ ξ ϕ    γ μ ψ ω    plutôt δ ou ω
injective symbol δ : SortK → TYPE;
symbol t : δ SortKItem;

injective symbol ω : SortK → TYPE;
symbol r : ω SortKItem;

injective symbol injK : SortK → TYPE;
symbol injG {From : SortK} {To : SortK} : injK From → injK To;

// For inductive type
symbol Prop : TYPE;
symbol P : Prop → TYPE;
builtin "Prop" ≔ Prop;
builtin "P" ≔ P;

//inductive Bool : K ≔
//  | true  : Bool
//  | false : Bool;

symbol SortBool : SortK;
symbol true : injK SortBool;
symbol false : injK SortBool;

symbol SortInt : SortK;

constant symbol zero : injK SortInt;
constant symbol succ : injK SortInt → injK SortInt;

builtin "0"  ≔ zero;
builtin "+1" ≔ succ;

//inductive Map : K ≔
//  | .Map  : Map
//  | _Map_ : Map → Map → Map;

symbol SortMap : SortK;
symbol Lbl'Stop'Map : injK SortMap;
symbol Lbl'Unds'Map'Unds' : injK SortMap → injK SortMap → injK SortMap;

//inductive Set : K ≔
//  | .Set  : Set
//  | _Set_ : Set → Set → Set;

symbol SortSet : SortK;
symbol Lbl'Stop'Set : injK SortSet;
symbol Lbl'Unds'Set'Unds' : injK SortSet → injK SortSet → injK SortSet;

//inductive List : K ≔
//  | .List  : List
//  | _List_ : List → List → List;

symbol SortList : SortK;
symbol Lbl'Stop'List : injK SortList;
symbol Lbl'Unds'List'Unds' : injK SortList → injK SortList → injK SortList;
