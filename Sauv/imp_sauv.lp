require open tests.kseq;
require open tests.Tests.prelude;
symbol SortList : SortK;
symbol SortFloat : SortK;
symbol SortMap : SortK;
symbol SortString : SortK;
symbol SortId : SortK;
symbol SortKConfigVar : SortK;
symbol SortInt : SortK;
symbol SortSet : SortK;
symbol SortKResult : SortK;
symbol SortCell : SortK;
symbol SortBool : SortK;
inductive SortEnvCellOpt : TYPE ≔
| LblnoEnvCell : SortEnvCellOpt;
inductive SortEnvCell : TYPE ≔
| Lbl'-LT-'env'-GT-' : injK SortMap → SortEnvCell;
inductive SortTCellOpt : TYPE ≔
| LblnoTCell : SortTCellOpt;
inductive SortKCellOpt : TYPE ≔
| LblnoKCell : SortKCellOpt;
inductive SortKCell : TYPE ≔
| Lbl'-LT-'k'-GT-' : SortK → SortKCell;
inductive SortTCellFragment : TYPE ≔
| Lbl'-LT-'T'-GT-'-fragment : SortKCellOpt → SortEnvCellOpt → SortTCellFragment;
inductive SortTCell : TYPE ≔
| Lbl'-LT-'T'-GT-' : SortKCell → SortEnvCell → SortTCell;
inductive SortAExp : TYPE ≔
| Lbl'UndsSlshUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp : SortAExp → SortAExp → SortAExp
| Lbl'Unds'-'UndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp : SortAExp → SortAExp → SortAExp
| Lbl'UndsPlusUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp : SortAExp → SortAExp → SortAExp;
inductive SortBExp : TYPE ≔
| Lblnot'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp : SortBExp → SortBExp
| Lbl'Unds'and'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp'Unds'BExp : SortBExp → SortBExp → SortBExp
| Lbl'Unds-LT-UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp : SortAExp → SortAExp → SortBExp
| Lbl'Unds-LT-EqlsUndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp : SortAExp → SortAExp → SortBExp;
inductive SortStmt : TYPE ≔
| Lbl'LBraRBraUnds'IMP-SYNTAX'Unds'Stmt : SortStmt
| Lbl'LBraUndsRBraUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt : SortStmt → SortStmt
| Lblwhile'Unds'do'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt : SortBExp → SortStmt → SortStmt
| Lblif'Unds'then'Unds'else'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt'Unds'Stmt : SortBExp → SortStmt → SortStmt → SortStmt
| Lbl'UndsUndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt'Unds'Stmt : SortStmt → SortStmt → SortStmt
| Lbl'UndsEqlsUndsSClnUnds'IMP-SYNTAX'Unds'Stmt'Unds'Id'Unds'AExp : injK SortId → SortAExp → SortStmt;
inductive SortIds : TYPE ≔
| Lbl'UndsCommUndsUnds'IMP-SYNTAX'Unds'Ids'Unds'Id'Unds'Ids : injK SortId → SortIds → SortIds
| Lbl'Stop'List'LBraQuotUndsCommUndsUnds'IMP-SYNTAX'Unds'Ids'Unds'Id'Unds'Ids'QuotRBraUnds'Ids : SortIds;
inductive SortPgm : TYPE ≔
| Lblvar'UndsSClnUndsUnds'IMP-SYNTAX'Unds'Pgm'Unds'Ids'Unds'Stmt : SortIds → SortStmt → SortPgm;
inductive SortKItem : TYPE ≔
| Lbl'Hash'freezernot'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp0'Unds' : SortKItem
| Lbl'Hash'freezerif'Unds'then'Unds'else'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt'Unds'Stmt0'Unds' : SortK → SortK → SortKItem
| Lbl'Hash'freezer'Unds'and'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp'Unds'BExp0'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'UndsEqlsUndsSClnUnds'IMP-SYNTAX'Unds'Stmt'Unds'Id'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds-LT-UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds-LT-UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp0'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds-LT-EqlsUndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds-LT-EqlsUndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp0'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'UndsSlshUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'UndsSlshUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp0'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds'-'UndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'Unds'-'UndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp0'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'UndsPlusUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp1'Unds' : SortK → SortKItem
| Lbl'Hash'freezer'UndsPlusUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp0'Unds' : SortK → SortKItem;

inductive SortGeneratedCounterCellOpt : TYPE ≔
| LblnoGeneratedCounterCell : SortGeneratedCounterCellOpt;
inductive SortGeneratedCounterCell : TYPE ≔
| Lbl'-LT-'generatedCounter'-GT-' : injK SortInt → SortGeneratedCounterCell;
inductive SortGeneratedTopCellFragment : TYPE ≔
| Lbl'-LT-'generatedTop'-GT-'-fragment : SortTCellOpt → SortGeneratedCounterCellOpt → SortGeneratedTopCellFragment;
inductive SortGeneratedTopCell : TYPE ≔
| Lbl'-LT-'generatedTop'-GT-' : SortTCell → SortGeneratedCounterCell → SortGeneratedTopCell;


symbol Lbl'Hash'if'UndsHash'then'UndsHash'else'UndsHash'fi'Unds'K-EQUAL-SYNTAX'Unds'Sort'Unds'Bool'Unds'Sort'Unds'Sort {SortSort : SortK} : injK SortBool → injK SortSort → injK SortSort → injK SortSort;
symbol Lbl'Stop'List : injK SortList;
symbol Lbl'Stop'Map : injK SortMap;
symbol Lbl'Stop'Set : injK SortSet;
symbol LblBase2String'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortString;
symbol LblFloat2String'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Float : injK SortFloat → injK SortString;
symbol LblFloat2String'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'Float'Unds'String : injK SortFloat → injK SortString → injK SortString;
symbol LblId2String'LParUndsRParUnds'ID-COMMON'Unds'String'Unds'Id : injK SortId → injK SortString;
symbol LblInt2String'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int : injK SortInt → injK SortString;
symbol LblList'Coln'get : SortList → SortInt → injK SortKItem;
symbol LblList'Coln'range : injK SortList → injK SortInt → injK SortInt → injK SortList;
symbol LblListItem : injK SortKItem → injK SortList;
symbol LblMap'Coln'lookup : injK SortMap → injK SortKItem → injK SortKItem;
symbol LblMap'Coln'update : injK SortMap → injK SortKItem → injK SortKItem → injK SortMap;
symbol LblSet'Coln'difference : injK SortSet → injK SortSet → injK SortSet;
symbol LblSet'Coln'in : injK SortKItem → injK SortSet → injK SortBool;
symbol LblSetItem : injK SortKItem → injK SortSet;
symbol LblString2Base'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'Int : injK SortString → injK SortInt → injK SortInt;
symbol LblString2Float'LParUndsRParUnds'STRING-COMMON'Unds'Float'Unds'String : injK SortString → injK SortFloat;
symbol LblString2Id'LParUndsRParUnds'ID-COMMON'Unds'Id'Unds'String : injK SortString → injK SortId;
symbol LblString2Int'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String : injK SortString → injK SortInt;
symbol Lbl'UndsPerc'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsAnd-'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsStar'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsPlus'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsPlus'String'UndsUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String : injK SortString → injK SortString → injK SortString;
symbol Lbl'Unds'-Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds'-Map'UndsUnds'MAP'Unds'Map'Unds'Map'Unds'Map : injK SortMap → injK SortMap → injK SortMap;
symbol Lbl'UndsSlsh'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds-LT--LT-'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds-LT-Eqls'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'Unds-LT-Eqls'Map'UndsUnds'MAP'Unds'Bool'Unds'Map'Unds'Map : injK SortMap → injK SortMap → injK SortBool;
symbol Lbl'Unds-LT-Eqls'Set'UndsUnds'SET'Unds'Bool'Unds'Set'Unds'Set : injK SortSet → injK SortSet → injK SortBool;
symbol Lbl'Unds-LT-Eqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
symbol Lbl'Unds-LT-'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'Unds-LT-'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
symbol Lbl'UndsEqlsSlshEqls'Bool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'UndsEqlsSlshEqls'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'UndsEqlsSlshEqls'K'Unds' : SortK → SortK → injK SortBool;
symbol Lbl'UndsEqlsSlshEqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
symbol Lbl'UndsEqlsEqls'Bool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'UndsEqlsEqls'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'UndsEqlsEqls'K'Unds' : SortK → SortK → injK SortBool;
symbol Lbl'UndsEqlsEqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
symbol Lbl'Unds-GT-Eqls'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'Unds-GT-Eqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
symbol Lbl'Unds-GT--GT-'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds-GT-'Int'Unds' : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'Unds-GT-'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String : injK SortString → injK SortString → injK SortBool;
left associative symbol Lbl'Unds'List'Unds' : injK SortList → injK SortList → injK SortList;
left associative commutative symbol Lbl'Unds'Map'Unds' : injK SortMap → injK SortMap → injK SortMap;
left associative commutative symbol Lbl'Unds'Set'Unds' : injK SortSet → injK SortSet → injK SortSet;
symbol Lbl'UndsLSqBUnds-LT-'-'UndsRSqBUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'KItem : injK SortList → injK SortInt → injK SortKItem → injK SortList;
symbol Lbl'UndsLSqBUnds-LT-'-undef'RSqB' : injK SortMap → injK SortKItem → injK SortMap;
symbol Lbl'UndsLSqBUndsRSqB'orDefault'UndsUnds'MAP'Unds'KItem'Unds'Map'Unds'KItem'Unds'KItem : injK SortMap → injK SortKItem → injK SortKItem → injK SortKItem;
symbol Lbl'UndsXor-Perc'Int'UndsUnds' : injK SortInt → injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsXor-'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds'andBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'andThenBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'divInt'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds'dividesInt'UndsUnds'INT-COMMON'Unds'Bool'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortBool;
symbol Lbl'Unds'impliesBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'in'UndsUnds'LIST'Unds'Bool'Unds'KItem'Unds'List : injK SortKItem → injK SortList → injK SortBool;
symbol Lbl'Unds'in'Unds'keys'LParUndsRParUnds'MAP'Unds'Bool'Unds'KItem'Unds'Map : injK SortKItem → injK SortMap → injK SortBool;
symbol Lbl'Unds'modInt'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'Unds'orBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'orElseBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'xorBool'Unds' : injK SortBool → injK SortBool → injK SortBool;
symbol Lbl'Unds'xorInt'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsPipe'-'-GT-Unds' : injK SortKItem → injK SortKItem → injK SortMap;
symbol Lbl'UndsPipe'Int'Unds' : injK SortInt → injK SortInt → injK SortInt;
symbol Lbl'UndsPipe'Set'UndsUnds'SET'Unds'Set'Unds'Set'Unds'Set : injK SortSet → injK SortSet → injK SortSet;
symbol LblabsInt'LParUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int : injK SortInt → injK SortInt;
symbol LblbitRangeInt'LParUndsCommUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortInt → injK SortInt;
symbol LblcategoryChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'String : injK SortString → injK SortString;
symbol Lblchoice'LParUndsRParUnds'MAP'Unds'KItem'Unds'Map : injK SortMap → injK SortKItem;
symbol Lblchoice'LParUndsRParUnds'SET'Unds'KItem'Unds'Set : injK SortSet → injK SortKItem;
symbol LblchrChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int : injK SortInt → injK SortString;
symbol LblcountAllOccurrences'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String : injK SortString → injK SortString → injK SortInt;
symbol LbldirectionalityChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'String : injK SortString → injK SortString;
symbol LblfillList'LParUndsCommUndsCommUndsCommUndsRParUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'Int'Unds'KItem : injK SortList → injK SortInt → injK SortInt → injK SortKItem → injK SortList;
symbol LblfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int : injK SortString → injK SortString → injK SortInt → injK SortInt;
symbol LblfindString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int : injK SortString → injK SortString → injK SortInt → injK SortInt;
symbol LblfreshId'LParUndsRParUnds'ID-COMMON'Unds'Id'Unds'Int : injK SortInt → injK SortId;
symbol LblfreshInt'LParUndsRParUnds'INT'Unds'Int'Unds'Int : injK SortInt → injK SortInt;
symbol LblgetGeneratedCounterCell : injK SortGeneratedTopCell → injK SortGeneratedCounterCell;
symbol LblinitEnvCell : injK SortEnvCell;
symbol LblinitGeneratedCounterCell : injK SortGeneratedCounterCell;
symbol LblinitGeneratedTopCell : injK SortMap → injK SortGeneratedTopCell;
symbol LblinitKCell : injK SortMap → injK SortKCell;
symbol LblinitTCell : injK SortMap → injK SortTCell;
symbol LblintersectSet'LParUndsCommUndsRParUnds'SET'Unds'Set'Unds'Set'Unds'Set : injK SortSet → injK SortSet → injK SortSet;
symbol LblisAExp : SortK → injK SortBool;
symbol LblisBExp : SortK → injK SortBool;
symbol LblisBool : SortK → injK SortBool;
symbol LblisCell : SortK → injK SortBool;
symbol LblisEnvCell : SortK → injK SortBool;
symbol LblisEnvCellOpt : SortK → injK SortBool;
symbol LblisFloat : SortK → injK SortBool;
symbol LblisGeneratedCounterCell : SortK → injK SortBool;
symbol LblisGeneratedCounterCellOpt : SortK → injK SortBool;
symbol LblisGeneratedTopCell : SortK → injK SortBool;
symbol LblisGeneratedTopCellFragment : SortK → injK SortBool;
symbol LblisId : SortK → injK SortBool;
symbol LblisIds : SortK → injK SortBool;
symbol LblisInt : SortK → injK SortBool;
symbol LblisK : SortK → injK SortBool;
symbol LblisKCell : SortK → injK SortBool;
symbol LblisKCellOpt : SortK → injK SortBool;
symbol LblisKConfigVar : SortK → injK SortBool;
symbol LblisKItem : SortK → injK SortBool;
symbol LblisKResult : SortK → injK SortBool;
symbol LblisList : SortK → injK SortBool;
symbol LblisMap : SortK → injK SortBool;
symbol LblisPgm : SortK → injK SortBool;
symbol LblisSet : SortK → injK SortBool;
symbol LblisStmt : SortK → injK SortBool;
symbol LblisString : SortK → injK SortBool;
symbol LblisTCell : SortK → injK SortBool;
symbol LblisTCellFragment : SortK → injK SortBool;
symbol LblisTCellOpt : SortK → injK SortBool;
symbol Lblkeys'LParUndsRParUnds'MAP'Unds'Set'Unds'Map : injK SortMap → injK SortSet;
symbol Lblkeys'Unds'list'LParUndsRParUnds'MAP'Unds'List'Unds'Map : injK SortMap → injK SortList;
symbol LbllengthString'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String : injK SortString → injK SortInt;
symbol Lbllog2Int'LParUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int : injK SortInt → injK SortInt;
symbol LblmakeList'LParUndsCommUndsRParUnds'LIST'Unds'List'Unds'Int'Unds'KItem : injK SortInt → injK SortKItem → injK SortList;
symbol LblmaxInt'LParUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortInt;
symbol LblminInt'LParUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortInt;
symbol LblnewUUID'Unds'STRING-COMMON'Unds'String : injK SortString;
symbol LblnotBool'Unds' : injK SortBool → injK SortBool;
symbol LblordChar'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String : injK SortString → injK SortInt;
symbol Lblproject'Coln'AExp : SortK → injK SortAExp;
symbol Lblproject'Coln'BExp : SortK → injK SortBExp;
symbol Lblproject'Coln'Bool : SortK → injK SortBool;
symbol Lblproject'Coln'Cell : SortK → injK SortCell;
symbol Lblproject'Coln'EnvCell : SortK → injK SortEnvCell;
symbol Lblproject'Coln'EnvCellOpt : SortK → injK SortEnvCellOpt;
symbol Lblproject'Coln'Float : SortK → injK SortFloat;
symbol Lblproject'Coln'GeneratedCounterCell : SortK → injK SortGeneratedCounterCell;
symbol Lblproject'Coln'GeneratedCounterCellOpt : SortK → injK SortGeneratedCounterCellOpt;
symbol Lblproject'Coln'GeneratedTopCell : SortK → injK SortGeneratedTopCell;
symbol Lblproject'Coln'GeneratedTopCellFragment : SortK → injK SortGeneratedTopCellFragment;
symbol Lblproject'Coln'Id : SortK → injK SortId;
symbol Lblproject'Coln'Ids : SortK → injK SortIds;
symbol Lblproject'Coln'Int : SortK → injK SortInt;
symbol Lblproject'Coln'K : SortK → SortK;
symbol Lblproject'Coln'KCell : SortK → injK SortKCell;
symbol Lblproject'Coln'KCellOpt : SortK → injK SortKCellOpt;
symbol Lblproject'Coln'KItem : SortK → injK SortKItem;
symbol Lblproject'Coln'KResult : SortK → injK SortKResult;
symbol Lblproject'Coln'List : SortK → injK SortList;
symbol Lblproject'Coln'Map : SortK → injK SortMap;
symbol Lblproject'Coln'Pgm : SortK → injK SortPgm;
symbol Lblproject'Coln'Set : SortK → injK SortSet;
symbol Lblproject'Coln'Stmt : SortK → injK SortStmt;
symbol Lblproject'Coln'String : SortK → injK SortString;
symbol Lblproject'Coln'TCell : SortK → injK SortTCell;
symbol Lblproject'Coln'TCellFragment : SortK → injK SortTCellFragment;
symbol Lblproject'Coln'TCellOpt : SortK → injK SortTCellOpt;
symbol LblrandInt'LParUndsRParUnds'INT'Unds'Int'Unds'Int : injK SortInt → injK SortInt;
symbol LblremoveAll'LParUndsCommUndsRParUnds'MAP'Unds'Map'Unds'Map'Unds'Set : injK SortMap → injK SortSet → injK SortMap;
symbol Lblreplace'LParUndsCommUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String'Unds'Int : injK SortString → injK SortString → injK SortString → injK SortInt → injK SortString;
symbol LblreplaceAll'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String : injK SortString → injK SortString → injK SortString → injK SortString;
symbol LblreplaceFirst'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String : injK SortString → injK SortString → injK SortString → injK SortString;
symbol LblrfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int : injK SortString → injK SortString → injK SortInt → injK SortInt;
symbol LblrfindString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int : injK SortString → injK SortString → injK SortInt → injK SortInt;
symbol LblsignExtendBitRangeInt'LParUndsCommUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int'Unds'Int : injK SortInt → injK SortInt → injK SortInt → injK SortInt;
symbol Lblsize'LParUndsRParUnds'LIST'Unds'Int'Unds'List : injK SortList → injK SortInt;
symbol Lblsize'LParUndsRParUnds'MAP'Unds'Int'Unds'Map : injK SortMap → injK SortInt;
symbol Lblsize'LParUndsRParUnds'SET'Unds'Int'Unds'Set : injK SortSet → injK SortInt;
symbol LblsrandInt'LParUndsRParUnds'INT'Unds'K'Unds'Int : injK SortInt → SortK;
symbol LblsubstrString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'Int'Unds'Int : injK SortString → injK SortInt → injK SortInt → injK SortString;
symbol LblupdateList'LParUndsCommUndsCommUndsRParUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'List : injK SortList → injK SortInt → injK SortList → injK SortList;
symbol LblupdateMap'LParUndsCommUndsRParUnds'MAP'Unds'Map'Unds'Map'Unds'Map : injK SortMap → injK SortMap → injK SortMap;
symbol Lblvalues'LParUndsRParUnds'MAP'Unds'List'Unds'Map : injK SortMap → injK SortList;
symbol Lbl'Tild'Int'Unds' : injK SortInt → injK SortInt;
rule TYPE ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lblif'Unds'then'Unds'else'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt'Unds'Stmt $VarB (Lbl'LBraUndsRBraUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt (Lbl'UndsUndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt'Unds'Stmt $VarS $Var'Unds'4)) Lbl'LBraRBraUnds'IMP-SYNTAX'Unds'Stmt)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule TYPE ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq $VarI $Var'Unds'DotVar2)) $Var'Unds'4) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortAExp} {SortKItem} (Lbl'UndsPlusUndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp (injG {SortInt} {SortAExp} $VarI1) (injG {SortInt} {SortAExp} $VarI2))) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortInt} {SortKItem} (Lbl'UndsPlus'Int'Unds' $VarI1 $VarI2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortAExp} {SortKItem} (Lbl'Unds'-'UndsUnds'IMP-SYNTAX'Unds'AExp'Unds'AExp'Unds'AExp (injG {SortInt} {SortAExp} $VarI1) (injG {SortInt} {SortAExp} $VarI2))) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortInt} {SortKItem} (Lbl'Unds'-Int'Unds' $VarI1 $VarI2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBExp} {SortKItem} (Lbl'Unds-LT-EqlsUndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp (injG {SortInt} {SortAExp} $VarI1) (injG {SortInt} {SortAExp} $VarI2))) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBool} {SortKItem} (Lbl'Unds-LT-Eqls'Int'Unds' $VarI1 $VarI2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBExp} {SortKItem} (Lbl'Unds-LT-UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'AExp'Unds'AExp (injG {SortInt} {SortAExp} $VarI1) (injG {SortInt} {SortAExp} $VarI2))) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBool} {SortKItem} (Lbl'Unds-LT-'Int'Unds' $VarI1 $VarI2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lbl'UndsEqlsUndsSClnUnds'IMP-SYNTAX'Unds'Stmt'Unds'Id'Unds'AExp $VarX (injG {SortInt} {SortAExp} $VarI))) $Var'Unds'DotVar2)) (Lbl'-LT-'env'-GT-' (Lbl'Unds'Map'Unds' (Lbl'UndsPipe'-'-GT-Unds' (injG {SortId} {SortKItem} $VarX) $Var'Unds'0) $Var'Unds'DotVar3))) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' $Var'Unds'DotVar2) (Lbl'-LT-'env'-GT-' (Lbl'Unds'Map'Unds' (Lbl'UndsPipe'-'-GT-Unds' (injG {SortId} {SortKItem} $VarX) (injG {SortInt} {SortKItem} $VarI)) $Var'Unds'DotVar3))) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lbl'UndsUndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt'Unds'Stmt $VarS1 $VarS2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} $VarS1) (kseq (injG {SortStmt} {SortKItem} $VarS2) $Var'Unds'DotVar2))) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule TYPE ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBool} {SortKItem} $Var'Unds'7) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBExp} {SortKItem} (Lbl'Unds'and'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp'Unds'BExp (injG {SortBool} {SortBExp} true) $VarB)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBExp} {SortKItem} $VarB) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lblif'Unds'then'Unds'else'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt'Unds'Stmt (injG {SortBool} {SortBExp} false) $Var'Unds'0 $VarS2)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} $VarS2) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lblif'Unds'then'Unds'else'UndsUnds'IMP-SYNTAX'Unds'Stmt'Unds'BExp'Unds'Stmt'Unds'Stmt (injG {SortBool} {SortBExp} true) $VarS1 $Var'Unds'0)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} $VarS1) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBExp} {SortKItem} (Lblnot'UndsUnds'IMP-SYNTAX'Unds'BExp'Unds'BExp (injG {SortBool} {SortBExp} $VarB))) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortBool} {SortKItem} (LblnotBool'Unds' $VarB)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortPgm} {SortKItem} (Lblvar'UndsSClnUndsUnds'IMP-SYNTAX'Unds'Pgm'Unds'Ids'Unds'Stmt Lbl'Stop'List'LBraQuotUndsCommUndsUnds'IMP-SYNTAX'Unds'Ids'Unds'Id'Unds'Ids'QuotRBraUnds'Ids $VarS)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} $VarS) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} (Lbl'LBraUndsRBraUnds'IMP-SYNTAX'Unds'Stmt'Unds'Stmt $VarS)) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} $VarS) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' (kseq (injG {SortStmt} {SortKItem} Lbl'LBraRBraUnds'IMP-SYNTAX'Unds'Stmt) $Var'Unds'DotVar2)) $Var'Unds'DotVar1) $Var'Unds'DotVar0 ↪ Lbl'-LT-'generatedTop'-GT-' (Lbl'-LT-'T'-GT-' (Lbl'-LT-'k'-GT-' $Var'Unds'DotVar2) $Var'Unds'DotVar1) $Var'Unds'DotVar0;
rule Lbl'Unds'List'Unds' (Lbl'Unds'List'Unds' $K1 $K2) $K3 ↪ Lbl'Unds'List'Unds' $K1 (Lbl'Unds'List'Unds' $K2 $K3);
rule Lbl'Unds'List'Unds' $K Lbl'Stop'List ↪ $K;
rule Lbl'Unds'List'Unds' Lbl'Stop'List $K ↪ $K;
rule Lbl'Unds'Map'Unds' (Lbl'Unds'Map'Unds' $K1 $K2) $K3 ↪ Lbl'Unds'Map'Unds' $K1 (Lbl'Unds'Map'Unds' $K2 $K3);
rule Lbl'Unds'Map'Unds' $K1 $K2 ↪ Lbl'Unds'Map'Unds' $K2 $K1;
rule Lbl'Unds'Map'Unds' $K Lbl'Stop'Map ↪ $K;
rule Lbl'Unds'Map'Unds' Lbl'Stop'Map $K ↪ $K;
rule Lbl'Unds'Set'Unds' (Lbl'Unds'Set'Unds' $K1 $K2) $K3 ↪ Lbl'Unds'Set'Unds' $K1 (Lbl'Unds'Set'Unds' $K2 $K3);
rule Lbl'Unds'Set'Unds' $K1 $K2 ↪ Lbl'Unds'Set'Unds' $K2 $K1;
rule Lbl'Unds'Set'Unds' $K $K ↪ $K;
rule Lbl'Unds'Set'Unds' $K Lbl'Stop'Set ↪ $K;
rule Lbl'Unds'Set'Unds' Lbl'Stop'Set $K ↪ $K;
