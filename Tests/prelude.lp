
// Module BASIC-K
symbol SortK : TYPE;
symbol SortKItem : SortK;

// Module INJ
injective symbol injK : SortK → TYPE;
symbol injG {From : SortK} {To : SortK} : injK From → injK To;

// For inductive type
symbol Prop : TYPE;
symbol P : Prop → TYPE;
builtin "Prop" ≔ Prop;
builtin "P" ≔ P;
