
// Translation of the module BASIC-K
symbol SortK : TYPE;
symbol SortKItem : SortK;

// Our injection between K and Dedukti
injective symbol δ : SortK → TYPE;

// Translation of the module INJ
symbol inj [From To : SortK] : δ From → δ To;

// Some builtins for Lambdapi
symbol Prop : TYPE;
symbol P : Prop → TYPE;
builtin "Prop" ≔ Prop;
builtin "P" ≔ P;

// BOOLEAN

symbol SortBool : SortK;
symbol true : δ SortBool;
symbol false : δ SortBool;

symbol notBool_ : δ SortBool → δ SortBool;
rule notBool_ true ↪ false;
rule notBool_ false ↪ true;

symbol _andBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _andBool_ true  $B    ↪ $B;
rule _andBool_ _     false ↪ false;
rule _andBool_ $B    true  ↪ $B;
rule _andBool_ false _     ↪ false;

symbol _andThenBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _andThenBool_ true  $K    ↪ $K;
rule _andThenBool_ _     false ↪ false;
rule _andThenBool_ $K    true  ↪ $K;
rule _andThenBool_ false _     ↪ false;

symbol _impliesBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _impliesBool_ true  $B    ↪ $B;
rule _impliesBool_ false _     ↪ true;
rule _impliesBool_ _     true  ↪ true;
rule _impliesBool_ $B    false ↪ notBool_ $B;

symbol _orBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _orBool_ true  _     ↪ true;
rule _orBool_ false $B    ↪ $B;
rule _orBool_ _     true  ↪ true;
rule _orBool_ $B    false ↪ $B;

symbol _orElseBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _orElseBool_ true  _     ↪ true;
rule _orElseBool_ false $K    ↪ $K;
rule _orElseBool_ _     true  ↪ true;
rule _orElseBool_ $K    false ↪ $K;

symbol _xorBool_ : δ SortBool → δ SortBool → δ SortBool;
rule _xorBool_ false $B    ↪ $B;
rule _xorBool_ $B    false ↪ $B;
rule _xorBool_ $B    $B    ↪ false;

symbol _==Bool_ : δ SortBool → δ SortBool → δ SortBool;
rule _==Bool_ false false ↪ true;
rule _==Bool_ true  true  ↪ true;
rule _==Bool_ false true  ↪ false;
rule _==Bool_ true  false ↪ false;

symbol _=Slsh=Bool_ : δ SortBool → δ SortBool → δ SortBool;
rule _=Slsh=Bool_ $B1 $B2 ↪ notBool_ (_==Bool_ $B1 $B2);


// Interlude

// For Viry encoding
symbol ♭Bool : SortK;
symbol ♭ : δ ♭Bool;
symbol ♭inj : δ SortBool → δ ♭Bool;

symbol _==K_ : SortK → SortK → δ SortBool; // TODO
// rule _==K_ (kseq (inj [SortBool] [SortKItem] $K1) dotk) (kseq (inj [SortBool] [SortKItem] $K2) dotk) ↪ _==Bool_ $K1 $K2;
// rule _==K_ (kseq (inj [SortInt] [SortKItem] $I1) dotk) (kseq (inj [SortInt] [SortKItem] $I2) dotk) ↪ _==Int_ $I1 $I2;

symbol _=Slsh=K_ : SortK → SortK → δ SortBool;
rule _=Slsh=K_ $K1 $K2 ↪ notBool_ (_==K_ $K1 $K2);


// INT

symbol SortInt : SortK; // TODO Fix
symbol zero : δ SortInt;
symbol succ : δ SortInt → δ SortInt;

builtin "0" ≔ zero;
builtin "+1" ≔ succ;

symbol _<Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _<Int_ 0 0 ↪ false;
rule _<Int_ 0 (succ _) ↪ true;
rule _<Int_ (succ $m) (succ $n) ↪ _<Int_ $m $n;
rule _<Int_ (succ _) 0 ↪ false;

symbol _>=Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _>=Int_ 0 0 ↪ true;
rule _>=Int_ 0 (succ _) ↪ false;
rule _>=Int_ (succ $m) (succ $n) ↪ _>=Int_ $m $n;
rule _>=Int_ (succ _) 0 ↪ true;

symbol _>Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _>Int_ 0 0 ↪ false;
rule _>Int_ 0 (succ _) ↪ false;
rule _>Int_ (succ $m) (succ $n) ↪ _>Int_ $m $n;
rule _>Int_ (succ _) 0 ↪ true;

symbol _<=Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _<=Int_ 0 0 ↪ true;
rule _<=Int_ 0 (succ _) ↪ true;
rule _<=Int_ (succ $m) (succ $n) ↪ _<=Int_ $m $n;
rule _<=Int_ (succ _) 0 ↪ false;

symbol _==Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _==Int_ 0 0 ↪ true;
rule _==Int_ 0 (succ _) ↪ false;
rule _==Int_ (succ $m) (succ $n) ↪ _==Int_ $m $n;
rule _==Int_ (succ _) 0 ↪ false;

symbol _=Slsh=Int_ : δ SortInt → δ SortInt → δ SortBool;
rule _=Slsh=Int_ $I1 $I2 ↪ notBool_ (_==Int_ $I1 $I2);

symbol _-Int_ : δ SortInt → δ SortInt → δ SortInt;
rule _-Int_ 0 0 ↪ 0;
rule _-Int_ 0 (succ _) ↪ 0;
rule _-Int_ (succ $m) (succ $n) ↪ _-Int_ $m $n;
rule _-Int_ (succ $n) 0 ↪ succ $n;

symbol _PlusInt_ : δ SortInt → δ SortInt → δ SortInt;
rule _PlusInt_ (succ $m) $n ↪ succ (_PlusInt_ $m $n);
rule _PlusInt_ 0 $n ↪ $n;

symbol _*Int_ : δ SortInt → δ SortInt → δ SortInt;
rule _*Int_ (succ $m) $n ↪ _PlusInt_ $n (_*Int_ $m $n);
rule _*Int_ 0 $n ↪ 0;

symbol minInt : δ SortInt → δ SortInt → δ SortInt;
symbol ♭minInt : δ SortInt → δ SortInt → δ ♭Bool → δ ♭Bool → δ SortInt;
rule minInt $x1 $x2 ↪ ♭minInt $x1 $x2 ♭ ♭;
rule ♭minInt $I1 $I2 (♭inj true) _ ↪ $I1;
rule ♭minInt $I1 $I2 ♭ $y0 ↪ ♭minInt $I1 $I2 (♭inj (_<=Int_ $I1 $I2)) $y0;
rule ♭minInt $I1 $I2 _ (♭inj true) ↪ $I2;
rule ♭minInt $I1 $I2 $y1 ♭ ↪ ♭minInt $I1 $I2 $y1 (♭inj (_>=Int_ $I1 $I2));

symbol maxInt : δ SortInt → δ SortInt → δ SortInt;
symbol ♭maxInt : δ SortInt → δ SortInt → δ ♭Bool → δ ♭Bool → δ SortInt;
rule maxInt $x1 $x2 ↪ ♭maxInt $x1 $x2 ♭ ♭;
rule ♭maxInt $I1 $I2 (♭inj true) _ ↪ $I2;
rule ♭maxInt $I1 $I2 ♭ $y0 ↪ ♭maxInt $I1 $I2 (♭inj (_<=Int_ $I1 $I2)) $y0;
rule ♭maxInt $I1 $I2 _ (♭inj true) ↪ $I1;
rule ♭maxInt $I1 $I2 $y1 ♭ ↪ ♭maxInt $I1 $I2 $y1 (♭inj (_>=Int_ $I1 $I2));


symbol _<<Int_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _>>Int_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol ~Int_ : δ SortInt → δ SortInt; // TODO

symbol _SlshInt_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _Xor-%Int__ : δ SortInt → δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _Xor-Int_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _PipeInt_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _%Int_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _And-Int_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol _xorInt_ : δ SortInt → δ SortInt → δ SortInt; // TODO

symbol absInt : δ SortInt → δ SortInt; // TODO


symbol  _modInt_ : δ SortInt → δ SortInt → δ SortInt; // TODO Check
symbol ♭_modInt_ : δ SortInt → δ SortInt → δ ♭Bool → δ SortInt;
rule _modInt_ $x1 $x2 ↪ ♭_modInt_ $x1 $x2 ♭;
rule ♭_modInt_ $I1 $I2 (♭inj true) ↪ _%Int_ (_PlusInt_ (_%Int_ $I1 (absInt $I2)) (absInt $I2)) (absInt $I2);
rule ♭_modInt_ $I1 $I2 ♭ ↪ ♭_modInt_ $I1 $I2 (♭inj (_=Slsh=Int_ $I2 0));


symbol _divInt_ : δ SortInt → δ SortInt → δ SortInt; // TODO Check
symbol ♭_divInt_ : δ SortInt → δ SortInt → δ ♭Bool → δ SortInt;
rule _divInt_ $x1 $x2 ↪ ♭_divInt_ $x1 $x2 ♭;
rule ♭_divInt_ $I1 $I2 (♭inj true) ↪ _SlshInt_ (_-Int_ $I1 (_modInt_ $I1 $I2)) $I2;
rule ♭_divInt_ $I1 $I2 ♭ ↪ ♭_divInt_ $I1 $I2 (♭inj (_=Slsh=Int_ $I2 0));

// symbol _dividesInt_ : δ SortInt → δ SortInt → δ SortBool; // TODO Check
// rule _dividesInt_ $I1 $I2 ↪ _==Int_ (_%Int_ $I2 $I1) 0;

// symbol freshInt_INT : δ SortInt → δ SortInt;
// rule freshInt_INT $I ↪ $I;

symbol bitRangeInt : δ SortInt → δ SortInt → δ SortInt → δ SortInt;
rule bitRangeInt $I $IDX $LEN ↪ _modInt_ (_>>Int_ $I $IDX) (_<<Int_ 1 $LEN);

symbol signExtendBitRangeInt : δ SortInt → δ SortInt → δ SortInt → δ SortInt;
rule signExtendBitRangeInt $I $IDX $LEN ↪ _-Int_ (_modInt_ (_PlusInt_ (bitRangeInt $I $IDX $LEN) (_<<Int_ 1 (_-Int_ $LEN 1))) (_<<Int_ 1 $LEN)) (_<<Int_ 1 (_-Int_ $LEN 1));



// STRING // TODO

symbol SortString : SortK;

symbol newUUID : δ SortString;

symbol _>=String_ : δ SortString → δ SortString → δ SortBool;

symbol _>String_ : δ SortString → δ SortString → δ SortBool;

symbol _<=String_ : δ SortString → δ SortString → δ SortBool;

symbol _<String_ : δ SortString → δ SortString → δ SortBool;

symbol _PlusString_ : δ SortString → δ SortString → δ SortString;

symbol replace : δ SortString → δ SortString → δ SortString → δ SortInt → δ SortString;

symbol replaceAll : δ SortString → δ SortString → δ SortString → δ SortString;

symbol replaceFirst : δ SortString → δ SortString → δ SortString → δ SortString;

symbol _==String_ : δ SortString → δ SortString → δ SortBool;

symbol _=Slsh=String_ : δ SortString → δ SortString → δ SortBool;

symbol categoryChar : δ SortString → δ SortString;

symbol directionalityChar : δ SortString → δ SortString;




symbol chrChar : δ SortInt → δ SortString;

symbol countAllOccurrences : δ SortString → δ SortString → δ SortInt;

symbol findChar : δ SortString → δ SortString → δ SortInt → δ SortInt;

symbol findString : δ SortString → δ SortString → δ SortInt → δ SortInt;

symbol lengthString : δ SortString → δ SortInt;

symbol log2Int : δ SortInt → δ SortInt;

symbol ordChar : δ SortString → δ SortInt;

symbol randInt_INT : δ SortInt → δ SortInt;

symbol srandInt_INT : δ SortInt → SortK;

symbol rfindChar : δ SortString → δ SortString → δ SortInt → δ SortInt;

symbol rfindString : δ SortString → δ SortString → δ SortInt → δ SortInt;

symbol substrString : δ SortString → δ SortInt → δ SortInt → δ SortString;


// IDENTIFIER

symbol SortId : SortK;

symbol _==Id_ : δ SortId → δ SortId → δ SortBool; // TODO

symbol Id2String : δ SortId → δ SortString; // TODO


symbol _==KItem_ : δ SortKItem → δ SortKItem → δ SortBool; // TODO

symbol a : δ SortId ;
symbol x : δ SortId ;
symbol y : δ SortId ;
symbol z : δ SortId ;

rule _==KItem_ (inj a) (inj x) ↪ false;
rule _==KItem_ (inj a) (inj y) ↪ false;
rule _==KItem_ (inj a) (inj z) ↪ false;

rule _==KItem_ (inj x) (inj y) ↪ false;
rule _==KItem_ (inj x) (inj z) ↪ false;
rule _==KItem_ (inj x) (inj a) ↪ false;

rule _==KItem_ (inj y) (inj a) ↪ false;
rule _==KItem_ (inj y) (inj x) ↪ false;
rule _==KItem_ (inj y) (inj z) ↪ false;

rule _==KItem_ (inj z) (inj a) ↪ false;
rule _==KItem_ (inj z) (inj x) ↪ false;
rule _==KItem_ (inj z) (inj y) ↪ false;

rule _==KItem_ (inj a) (inj a) ↪ true;
rule _==KItem_ (inj x) (inj x) ↪ true;
rule _==KItem_ (inj y) (inj y) ↪ true;
rule _==KItem_ (inj z) (inj z) ↪ true;




// CONVERSION

symbol Base2String : δ SortInt → δ SortInt → δ SortString; // TODO

symbol String2Base : δ SortString → δ SortInt → δ SortInt; // TODO

symbol Int2String  : δ SortInt → δ SortString; // TODO

symbol String2Int : δ SortString → δ SortInt; // TODO

symbol String2Id : δ SortString → δ SortId; // TODO

// SET // TODO

symbol SortSet : SortK;
symbol StopSet : δ SortSet;
symbol _Set_ : δ SortSet → δ SortSet → δ SortSet;

symbol SetColndifference : δ SortSet → δ SortSet → δ SortSet;

symbol SetColnin : δ SortKItem → δ SortSet → δ SortBool;

symbol SetItem : δ SortKItem → δ SortSet;

symbol choice_SET : δ SortSet → δ SortKItem;

symbol _PipeSet__SET : δ SortSet → δ SortSet → δ SortSet;
rule _PipeSet__SET $S1 $S2 ↪ _Set_ $S1 (SetColndifference $S2 $S1);


symbol intersectSet_SET : δ SortSet → δ SortSet → δ SortSet;

symbol _<=Set__SET : δ SortSet → δ SortSet → δ SortBool;

symbol size_SET : δ SortSet → δ SortInt;

// LIST // TODO

symbol SortList : SortK;
symbol StopList : δ SortList;
symbol _List_ : δ SortList → δ SortList → δ SortList;

symbol ListColnget : δ SortList → δ SortInt → δ SortKItem;

symbol ListColnrange : δ SortList → δ SortInt → δ SortInt → δ SortList;

symbol _LSqB_<-_RSqB_LIST : δ SortList → δ SortInt → δ SortKItem → δ SortList;

symbol ListItem : δ SortKItem → δ SortList;

symbol _in__LIST : δ SortKItem → δ SortList → δ SortBool;

symbol size_LIST : δ SortList → δ SortInt;

symbol fillList_LIST : δ SortList → δ SortInt → δ SortInt → δ SortKItem → δ SortList;

symbol makeList_LIST : δ SortInt → δ SortKItem → δ SortList;

symbol updateList_LIST : δ SortList → δ SortInt → δ SortList → δ SortList;


// MAP

symbol SortMap : SortK;
symbol StopMap : δ SortMap;
symbol _Map_ : δ SortMap → δ SortMap → δ SortMap;

rule _Map_ $M      StopMap ↪ $M ;
rule _Map_ StopMap $M      ↪ $M ;


symbol _Pipe->_ : δ SortKItem → δ SortKItem → δ SortMap; // Singleton

symbol _in_keys_MAP : δ SortKItem → δ SortMap → δ SortBool;
symbol ♭_in_keys_Map : δ SortKItem → δ SortMap → δ ♭Bool → δ ♭Bool → δ SortBool;
rule _in_keys_MAP $x1 $x5 ↪ ♭_in_keys_Map $x1 $x5 ♭ ♭;

// Empty map
rule ♭_in_keys_Map $_0 StopMap _ _ ↪ false;

// Singleton map
rule ♭_in_keys_Map $X (_Pipe->_ $Y _) _ _ ↪ _==KItem_ $X $Y;

// Map with at least 2 elements
rule ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y _) $M) (♭inj true) _ ↪ _in_keys_MAP $X $M;
rule ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y $_0) $M) ♭ $y0
 ↪  ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y $_0) $M) (♭inj (notBool_ (_==KItem_ $X $Y))) $y0;

rule ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y _) _) _ (♭inj true) ↪ true;
rule ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y $_0) $_1) $y1 ♭
 ↪  ♭_in_keys_Map $X (_Map_ (_Pipe->_ $Y $_0) $_1) $y1 (♭inj (_==KItem_ $X $Y));



symbol MapColnlookup : δ SortMap → δ SortKItem → δ SortKItem;
symbol ♭lookup_Map : δ SortKItem → δ SortMap → δ ♭Bool → δ ♭Bool → δ SortKItem;
rule MapColnlookup $x1 $x5 ↪ ♭lookup_Map $x5 $x1 ♭ ♭;

// Singleton map
rule ♭lookup_Map $X (_Pipe->_ $Y $I) (♭inj true) _ ↪ $I;
rule ♭lookup_Map $X (_Pipe->_ $Y $I) ♭ $y0
 ↪  ♭lookup_Map $X (_Pipe->_ $Y $I) (♭inj (_==KItem_ $X $Y)) $y0;

// Map with at least 2 elements
rule ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $I) _) (♭inj true) _ ↪ $I;
rule ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $I) $M) ♭ $y0
 ↪  ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $I) $M) (♭inj (_==KItem_ $X $Y)) $y0;

rule ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $_0) $M) _ (♭inj true) ↪ MapColnlookup $M $X;
rule ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $_0) $M) $y1 ♭
 ↪  ♭lookup_Map $X (_Map_ (_Pipe->_ $Y $_0) $M) $y1 (♭inj (notBool_ (_==KItem_ $X $Y)));


symbol MapColnupdate : δ SortMap → δ SortKItem → δ SortKItem → δ SortMap;
//symbol ♭update_tmp : δ SortKItem → δ SortKItem → δ SortMap → δ ♭Bool → δ ♭Bool → δ SortKItem → δ SortBool → δ SortMap;
symbol ♭update_ : δ SortKItem → δ SortKItem → δ SortMap → δ ♭Bool → δ SortMap;
rule MapColnupdate StopMap $X $V ↪ (_Pipe->_ $X $V);
rule MapColnupdate (_Pipe->_ $Y $W) $X $V ↪ ♭update_ $X $V (_Pipe->_ $Y $W) ♭;
rule MapColnupdate (_Map_ (_Pipe->_ $Y $W) $M) $X $V ↪ // ♭update_tmp $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭ ♭ $Y (_==KItem_ $X $Y);
  ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭;
//rule ♭update_tmp $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭ ♭ $Y true  ↪ (_Map_ (_Pipe->_ $Y $V) $M);
//rule ♭update_tmp $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭ ♭ $Y false ↪ (_Map_ (_Pipe->_ $Y $W) (♭update_ $X $V $M ♭ ♭ $Y);

// Singleton map
//rule ♭update_ $X $V (_Pipe->_ $Y $W) (♭inj true) _ ↪ (_Pipe->_ $Y $W);
//rule ♭update_ $X $V (_Pipe->_ $Y $W)  ♭ $y0
// ↪  ♭update_ $X $V (_Pipe->_ $Y $W) (♭inj (notBool_ (_==KItem_ $X $Y))) $y0;

rule ♭update_ $X $V (_Pipe->_ $Y $W) ♭
 ↪  ♭update_ $X $V (_Pipe->_ $Y $W) (♭inj (_==KItem_ $X $Y));
rule ♭update_ $X $V (_Pipe->_ $Y $W) (♭inj true)  ↪ (_Pipe->_ $X $V);
rule ♭update_ $X $V (_Pipe->_ $Y $W) (♭inj false) ↪ _Map_ (_Pipe->_ $Y $W) (_Pipe->_ $X $V);

// Map with at least 2 elements
//rule ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) (♭inj true) _ ↪ _Map_ (_Pipe->_ $Y $W) (MapColnupdate $M $X $V);
//rule ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭ $y0
// ↪  ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) (♭inj (notBool_ (_==KItem_ $X $Y))) $y0;

rule ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) ♭
 ↪  ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) (♭inj (_==KItem_ $X $Y));
rule ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) (♭inj true)  ↪ _Map_ (_Pipe->_ $X $V) $M;
rule ♭update_ $X $V (_Map_ (_Pipe->_ $Y $W) $M) (♭inj false) ↪ _Map_ (_Pipe->_ $Y $W) (♭update_ $X $V $M ♭);

symbol choice_MAP : δ SortMap → δ SortKItem; // TODO

symbol _-Map__MAP : δ SortMap → δ SortMap → δ SortMap; // TODO

symbol _<=Map__MAP : δ SortMap → δ SortMap → δ SortBool; // TODO

symbol _LSqB_<-undefRSqB : δ SortMap → δ SortKItem → δ SortMap; // TODO

symbol _LSqB_RSqBorDefault__MAP : δ SortMap → δ SortKItem → δ SortKItem → δ SortKItem; // TODO

symbol keys_list_MAP : δ SortMap → δ SortList; // TODO

symbol keys_MAP : δ SortMap → δ SortSet; // TODO

symbol removeAll_MAP : δ SortMap → δ SortSet → δ SortMap; // TODO

symbol size_MAP : δ SortMap → δ SortInt; // TODO

symbol updateMap_MAP : δ SortMap → δ SortMap → δ SortMap; // TODO

symbol values_MAP : δ SortMap → δ SortList; // TODO


// FLOAT

symbol SortFloat : SortK;
symbol String2Float : δ SortString → δ SortFloat; // TODO
