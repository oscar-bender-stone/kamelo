require open tests.minus-mini;
require open tests.kseq;
require open tests.Tests.prelude;

symbol DotVarK : SortK;
symbol DotVar : injK SortGeneratedCounterCell;

// Fisrt useful symbol
symbol configuration (in_k :injK SortAExp) : injK SortGeneratedTopCell
 ≔ <generatedTop> (<T> (<k> (kseq (injG in_k) DotVarK))) DotVar ;
// OR symbol configuration (in_k :injK SortKItem) : injK SortGeneratedTopCell;
// rule configuration $in_k ↪ <generatedTop> (<T> (<k> (kseq (injG $in_k) DotVarK))) DotVar;

// 5 - 5 donne 0
compute configuration (_-__ (injG 5_) (injG 5_));
// (5 - 5) - 0 ne se réduit pas
compute configuration (_-__ (_-__ (injG 5_) (injG 5_)) (injG 0_));
// (5 - 5) - (5 - 5) donne 0
compute configuration (_-__ (_-__ (injG 5_) (injG 5_)) (_-__ (injG 5_) (injG 5_)));
