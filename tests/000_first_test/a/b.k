module SORT-K 
  syntax K [hook(K.K)]
endmodule

module BASIC-K 
  imports SORT-BOOL
  syntax KItem [hook(K.KItem)]
  syntax K  ::= KItem
  syntax KConfigVar [token]
endmodule

module KSTRING 
  syntax KString  ::=
    r"[\\\"](([^\\\"\\n\\r\\\\])|([\\\\][nrtf\\\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\\\"]" [token]
endmodule

module BUILTIN-ID-TOKENS 
  syntax #LowerId  ::=
    r"[a-z][a-zA-Z0-9]*" [prec(2), token]
  syntax #UpperId  ::=
    r"[A-Z][a-zA-Z0-9]*" [prec(2), token]
endmodule

module SORT-KBOTT 
  imports SORT-K
  syntax KBott 
endmodule

module KAST 
  imports BASIC-K
  imports SORT-KBOTT
  imports KSTRING
  imports BUILTIN-ID-TOKENS
endmodule

module ML-SYNTAX 
  imports SORT-K
  syntax {Sort1, Sort2} Sort2  ::=
    "#Ceil" "(" Sort1 ")" [klabel(#Ceil), mlOp, mlUnary, symbol]
endmodule

module KSEQ-SYMBOLIC
  imports KAST
  syntax #KVariable  ::=
    r"(?<![A-Za-z0-9_\\$!\\?@])(\\!|\\?|@)?([A-Z][A-Za-z0-9'_]*|_|_[A-Z][A-Za-z0-9'_]*)" [prec(1), token]
  | #UpperId [token]
  syntax KConfigVar  ::=
    r"(?<![A-Za-z0-9_\\$!\\?@])(\\$)([A-Z][A-Za-z0-9'_]*)" [token]
  syntax KBott  ::= KConfigVar
endmodule

module KCELLS 
  imports KAST
  syntax Cell 
  syntax Bag  ::=
    Bag Bag [assoc, klabel(#cells), left, symbol, unit(#cells)]
  | ".Bag" [klabel(#cells), symbol]
  | ".::Bag" [klabel(#cells), symbol]
  | Cell 
  syntax Bag  ::= "(" Bag ")" [bracket]
  syntax KItem  ::= Bag
  syntax Bag  ::= KBott
endmodule

module RULE-CELLS 
endmodule

module CONFIG-CELLS 
  imports KCELLS
  syntax #CellName  ::=
    r"[a-zA-Z][a-zA-Z0-9\\-]*" [prec(1), token]
  | #LowerId [token]
  | #UpperId [token]
  syntax Cell  ::=
    "<" #CellName #CellProperties ">" K "</" #CellName ">" [klabel(#configCell), symbol]
  syntax Cell  ::=
    "<" #CellName "/>" [klabel(#externalCell), symbol]
  syntax #CellProperties  ::=
    #CellProperty #CellProperties [klabel(#cellPropertyList), symbol]
  | "" [klabel(#cellPropertyListTerminator), symbol]
  syntax #CellProperty  ::=
    #CellName "=" KString [klabel(#cellProperty), symbol]
endmodule

module REQUIRES-ENSURES 
  imports BASIC-K
  syntax #RuleBody  ::= K
  syntax #RuleContent  ::=
    #RuleBody [klabel(#ruleNoConditions), symbol]
endmodule

module AUTO-CASTS
endmodule

module K 
  imports KSEQ-SYMBOLIC
  imports REQUIRES-ENSURES
  imports AUTO-CASTS
endmodule

module K-TERM 
endmodule

module DEFAULT-LAYOUT 
  syntax #Layout  ::=
    r"(\\/\\*([^\\*]|(\\*+([^\\*\\/])))*\\*+\\/)" 
  | r"(\\/\\/[^\\n\\r]*)" 
  | r"([\\ \\n\\r\\t])" 
endmodule

module DEFAULT-CONFIGURATION 
  imports BASIC-K
  configuration <k> $PGM:K </k> 
endmodule

module MAP
  syntax Map [hook(MAP.Map)]
  syntax Map  ::=
    Map Map [assoc, comm, element(_|->_), format(%1%n%2), function, hook(MAP.concat), index(0), klabel(_Map_), left, symbol, unit(.Map)]
  syntax Map  ::=
    ".Map" [function, functional, hook(MAP.unit), klabel(.Map), latex(\dotCt{Map}), symbol]
  syntax Map  ::=
    KItem "|->" KItem [function, functional, hook(MAP.element), klabel(_|->_), latex({#1}\mapsto{#2}), symbol]
  syntax KItem  ::=
    Map "[" KItem "]" [function, hook(MAP.lookup), klabel(Map:lookup), symbol]
  syntax Int  ::=
    "size" "(" Map ")" [function, functional, hook(MAP.size), klabel(sizeMap)]
endmodule


module SET [not-lr1]
endmodule

module LIST [not-lr1]
endmodule

module SORT-BOOL
  syntax Bool [hook(BOOL.Bool)]
endmodule

module INT-SYNTAX
endmodule

module ID-SYNTAX-PROGRAM-PARSING
endmodule

module K-REFLECTION
endmodule

module K-REFLECTION-SYMBOLIC [kast, symbolic]
endmodule

module STDIN-STREAM
endmodule

module STDOUT-STREAM 
endmodule



module A-SYNTAX 
  syntax A  ::=
    "a" 
endmodule

module A 
  imports A-SYNTAX
  syntax KResult  ::=
    A 
endmodule
