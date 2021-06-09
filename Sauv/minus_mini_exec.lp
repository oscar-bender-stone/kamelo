require open tests.minus-mini;
require open tests.kseq;
require open tests.Tests.prelude;

// First computation
type 1_MINUS-MINI-SYNTAX_Int;
type _-__MINUS-MINI-SYNTAX_AExp_AExp_AExp;
type kseq;
type <k>;
type <T>;
type <generatedTop>;
symbol DotVarK : SortK;
symbol DotVar : injK SortGeneratedCounterCell;
type injK;
type SortKItem;
type _-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 1_MINUS-MINI-SYNTAX_Int) (injG 1_MINUS-MINI-SYNTAX_Int);
type  (kseq (injG {SortAExp} {SortKItem} (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 1_MINUS-MINI-SYNTAX_Int) (injG 1_MINUS-MINI-SYNTAX_Int))));
type <generatedTop> (<T> (<k> (kseq (injG {SortAExp} {SortKItem} (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 1_MINUS-MINI-SYNTAX_Int) (injG 1_MINUS-MINI-SYNTAX_Int))) DotVarK))) DotVar;
compute <generatedTop> (<T> (<k> (kseq (injG {SortAExp} {SortKItem} (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 1_MINUS-MINI-SYNTAX_Int) (injG 1_MINUS-MINI-SYNTAX_Int))) DotVarK))) DotVar;

// Fisrt useful symbol
symbol configuration (in_k :injK SortAExp) : injK SortGeneratedTopCell
 ≔ <generatedTop> (<T> (<k> (kseq (injG in_k) DotVarK))) DotVar ;
// OR symbol configuration (in_k :injK SortKItem) : injK SortGeneratedTopCell;
// rule configuration $in_k ↪ <generatedTop> (<T> (<k> (kseq (injG $in_k) DotVarK))) DotVar;

// 5 - 5 donne 0
compute configuration (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 5_MINUS-MINI-SYNTAX_Int) (injG 5_MINUS-MINI-SYNTAX_Int));
// (5 - 5) - 0 ne se réduit pas
compute configuration (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp
                         (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 5_MINUS-MINI-SYNTAX_Int) (injG 5_MINUS-MINI-SYNTAX_Int))
                         (injG 0_MINUS-MINI-SYNTAX_Int));
// (5 - 5) - (5 - 5) donne 0
compute configuration (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp
                         (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 5_MINUS-MINI-SYNTAX_Int) (injG 5_MINUS-MINI-SYNTAX_Int))
                         (_-__MINUS-MINI-SYNTAX_AExp_AExp_AExp (injG 5_MINUS-MINI-SYNTAX_Int) (injG 5_MINUS-MINI-SYNTAX_Int)));
