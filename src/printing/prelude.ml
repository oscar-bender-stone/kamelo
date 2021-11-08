
open LP.Syntax
open Interface.LP_p_term
open Interface.K_prelude

let hooked_sort =
  [ "SortList"   (* hook{}("LIST.List")     *)
  ; "SortFloat"  (* hook{}("FLOAT.Float")   *)
  ; "SortMap"    (* hook{}("MAP.Map")       *)
  ; "SortString" (* hook{}("STRING.String") *)
  ; "SortInt"    (* hook{}("INT.Int")       *)
  ; "SortSet"    (* hook{}("SET.Set")       *)
  ; "SortBool"   (* hook{}("BOOL.Bool")     *)
  ]

let hooked_symbol =
  [ ("Lbl'Hash'if'UndsHash'then'UndsHash'else'UndsHash'fi'Unds'K-EQUAL-SYNTAX'Unds'Sort'Unds'Bool'Unds'Sort'Unds'Sort",
     ["SortSort" (* parametre {  } *); "SortBool"; "SortSort"; "SortSort"; "SortSort"]) (* klabel ? hook{}("KEQUAL.ite") *)
  ; ("Lbl'Stop'List",
     ["SortList"]) (* klabel ? hook{}("LIST.unit") *)
  ; ("Lbl'Stop'Map",
     ["SortMap"])  (* klabel ? hook{}("MAP.unit")  *)
  ; ("Lbl'Stop'Set",
     ["SortSet"])  (* klabel ? hook{}("SET.unit")  *)
  ; ("LblBase2String'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"; "SortString"]) (* klabel ?  hook{}("STRING.base2string") *)
  ; ("LblFloat2String'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Float",
     ["SortFloat"; "SortString"]) (* klabel{}("Float2String"), hook{}("STRING.float2string") *)
  ; ("LblFloat2String'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'Float'Unds'String",
     ["SortFloat"; "SortString"; "SortString"]) (* klabel{}("FloatFormat"), hook{}("STRING.floatFormat") *)
  ; ("LblId2String'LParUndsRParUnds'ID-COMMON'Unds'String'Unds'Id",
     ["SortId"; "SortString"]) (* klabel{}("Id2String"), hook{}("STRING.token2string") *)
  ; ("LblInt2String'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int",
     ["SortInt"; "SortString"]) (* klabel{}("Int2String"), hook{}("STRING.int2string") *)
  ; ("LblList'Coln'get",
     ["SortList"; "SortInt"; "SortKItem"]) (* klabel{}("List:get"), hook{}("LIST.get") *)
  ; ("LblList'Coln'range",
     ["SortList"; "SortInt"; "SortInt"; "SortList"]) (* klabel{}("List:range"), hook{}("LIST.range") *)
  ; ("LblListItem",
     ["SortKItem"; "SortList"]) (* klabel{}("ListItem"), hook{}("LIST.element") *)
  ; ("LblMap'Coln'lookup",
     ["SortMap"; "SortKItem"; "SortKItem"]) (* klabel{}("Map:lookup"), hook{}("MAP.lookup") *)
  ; ("LblMap'Coln'update",
     ["SortMap"; "SortKItem"; "SortKItem"; "SortMap"]) (* klabel{}("Map:update"), hook{}("MAP.update") *)
  ; ("LblSet'Coln'difference",
     ["SortSet"; "SortSet"; "SortSet"]) (* klabel{}("Set:difference"), hook{}("SET.difference") *)
  ; ("LblSet'Coln'in",
     ["SortKItem"; "SortSet"; "SortBool"]) (* klabel{}("Set:in"), hook{}("SET.in") *)
  ; ("LblSetItem",
     ["SortKItem"; "SortSet"]) (* klabel{}("SetItem"), hook{}("SET.element") *)
  ; ("LblString2Base'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'Int",
     ["SortString"; "SortInt"; "SortInt"]) (* klabel{}("String2Base"), hook{}("STRING.string2base") *)
  ; ("LblString2Float'LParUndsRParUnds'STRING-COMMON'Unds'Float'Unds'String",
     ["SortString"; "SortFloat"]) (* klabel{}("String2Float"), hook{}("STRING.string2float") *)
  ; ("LblString2Id'LParUndsRParUnds'ID-COMMON'Unds'Id'Unds'String",
     ["SortString"; "SortId"]) (* klabel{}("String2Id"), hook{}("STRING.string2token") *)
  ; ("LblString2Int'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String",
     ["SortString"; "SortInt"]) (* klabel{}("String2Int") hook{}("STRING.string2int") *)
  ; ("Lbl'UndsPerc'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_%Int_"), hook{}("INT.tmod") *)
  ; ("Lbl'UndsAnd-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_&Int_"), hook{}("INT.and") *)
  ; ("Lbl'UndsStar'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_*Int_"), hook{}("INT.mul") *)
  ; ("Lbl'UndsPlus'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_+Int_"), hook{}("INT.add") *)
  ; ("Lbl'UndsPlus'String'UndsUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortString"]) (* hook{}("STRING.concat") *)
  ; ("Lbl'Unds'-Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_-Int_"), hook{}("INT.sub") *)
  ; ("Lbl'Unds'-Map'UndsUnds'MAP'Unds'Map'Unds'Map'Unds'Map",
     ["SortMap"; "SortMap"; "SortMap"]) (* hook{}("MAP.difference") *)
  ; ("Lbl'UndsSlsh'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_/Int_"), hook{}("INT.tdiv") *)
  ; ("Lbl'Unds-LT--LT-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_<<Int_"), hook{}("INT.shl") *)
  ; ("Lbl'Unds-LT-Eqls'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"])  (* klabel{}("_<=Int_"), hook{}("INT.le") *)
  ; ("Lbl'Unds-LT-Eqls'Map'UndsUnds'MAP'Unds'Bool'Unds'Map'Unds'Map",
     ["SortMap"; "SortMap"; "SortBool"]) (* hook{}("MAP.inclusion") *)
  ; ("Lbl'Unds-LT-Eqls'Set'UndsUnds'SET'Unds'Bool'Unds'Set'Unds'Set",
     ["SortSet"; "SortSet"; "SortBool"]) (* hook{}("SET.inclusion") *)
  ; ("Lbl'Unds-LT-Eqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"]) (* hook{}("STRING.le") *)
  ; ("Lbl'Unds-LT-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"])  (* klabel{}("_<Int_"), hook{}("INT.lt") *)
  ; ("Lbl'Unds-LT-'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"]) (* hook{}("STRING.lt") *)
  ; ("Lbl'UndsEqlsSlshEqls'Bool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* hook{}("BOOL.ne") *)
  ; ("Lbl'UndsEqlsSlshEqls'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"]) (* hook{}("INT.ne") *)
  ; ("Lbl'UndsEqlsSlshEqls'K'Unds'",
     ["SortK"; "SortK"; "SortBool"]) (* hook{}("KEQUAL.ne") *)
  ; ("Lbl'UndsEqlsSlshEqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"])   (* hook{}("STRING.ne") *)
  ; ("Lbl'UndsEqlsEqls'Bool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* hook{}("BOOL.eq") *)
  ; ("Lbl'UndsEqlsEqls'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"]) (* klabel{}("_==Int_"), hook{}("INT.eq") *)
  ; ("Lbl'UndsEqlsEqls'K'Unds'",
     ["SortK"; "SortK"; "SortBool"]) (* klabel{}("_==K_"), hook{}("KEQUAL.eq") *)
  ; ("Lbl'UndsEqlsEqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"]) (* hook{}("STRING.eq") *)
  ; ("Lbl'Unds-GT-Eqls'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"]) (* klabel{}("_>=Int_"), hook{}("INT.ge") *)
  ; ("Lbl'Unds-GT-Eqls'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"]) (* hook{}("STRING.ge") *)
  ; ("Lbl'Unds-GT--GT-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_>>Int_"), hook{}("INT.shr") *)
  ; ("Lbl'Unds-GT-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortBool"]) (* klabel{}("_>Int_"), hook{}("INT.gt") *)
  ; ("Lbl'Unds-GT-'String'UndsUnds'STRING-COMMON'Unds'Bool'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortBool"]) (* hook{}("STRING.gt") *)
  ; ("Lbl'Unds'List'Unds'",
     ["SortList"; "SortList"; "SortList"]) (* klabel{}("_List_"), hook{}("LIST.concat") *)
  ; ("Lbl'Unds'Map'Unds'",
     ["SortMap"; "SortMap"; "SortMap"]) (* klabel{}("_Map_"), hook{}("MAP.concat") *)
  ; ("Lbl'Unds'Set'Unds'",
     ["SortSet"; "SortSet"; "SortSet"]) (* klabel{}("_Set_"), hook{}("SET.concat") *)
  ; ("Lbl'UndsLSqBUnds-LT-'-'UndsRSqBUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'KItem",
     ["SortList"; "SortInt"; "SortKItem"; "SortList"]) (* klabel{}("List:set"), hook{}("LIST.update") *)
  ; ("Lbl'UndsLSqBUnds-LT-'-undef'RSqB'",
     ["SortMap"; "SortKItem"; "SortMap"]) (* klabel{}("_[_<-undef]"), hook{}("MAP.remove") *)
  ; ("Lbl'UndsLSqBUndsRSqB'orDefault'UndsUnds'MAP'Unds'KItem'Unds'Map'Unds'KItem'Unds'KItem",
     ["SortMap"; "SortKItem"; "SortKItem"; "SortKItem"]) (* klabel{}("Map:lookupOrDefault"), hook{}("MAP.lookupOrDefault") *)
  ; ("Lbl'UndsXor-Perc'Int'UndsUnds'",
     ["SortInt"; "SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_^%Int__"), hook{}("INT.powmod") *)
  ; ("Lbl'UndsXor-'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_^Int_"), hook{}("INT.pow") *)
  ; ("Lbl'Unds'andBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* klabel{}("_andBool_"), hook{}("BOOL.and") *)
  ; ("Lbl'Unds'andThenBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* klabel{}("_andThenBool_"), hook{}("BOOL.andThen") *)
  ; ("Lbl'Unds'divInt'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_divInt_"), hook{}("INT.ediv") *)
  ; ("Lbl'Unds'impliesBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* klabel{}("_impliesBool_"), hook{}("BOOL.implies") *)
  ; ("Lbl'Unds'in'UndsUnds'LIST'Unds'Bool'Unds'KItem'Unds'List",
     ["SortKItem"; "SortList"; "SortBool"]) (* klabel{}("_inList_"), hook{}("LIST.in") *)
  ; ("Lbl'Unds'in'Unds'keys'LParUndsRParUnds'MAP'Unds'Bool'Unds'KItem'Unds'Map",
     ["SortKItem"; "SortMap"; "SortBool"]) (* hook{}("MAP.in_keys") *)
  ; ("Lbl'Unds'modInt'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_modInt_"), hook{}("INT.emod") *)
  ; ("Lbl'Unds'orBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"])  (* klabel{}("_orBool_"), hook{}("BOOL.or") *)
  ; ("Lbl'Unds'orElseBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* klabel{}("_orElseBool_"), hook{}("BOOL.orElse") *)
  ; ("Lbl'Unds'xorBool'Unds'",
     ["SortBool"; "SortBool"; "SortBool"]) (* klabel{}("_xorBool_"), hook{}("BOOL.xor") *)
  ; ("Lbl'Unds'xorInt'Unds'",
     ["SortInt"; "SortInt"; "SortInt"]) (* klabel{}("_xorInt_"), hook{}("INT.xor") *)
  ; ("Lbl'UndsPipe'-'-GT-Unds'",
     ["SortKItem"; "SortKItem"; "SortMap"]) (* klabel{}("_|->_"), hook{}("MAP.element") *)
  ; ("Lbl'UndsPipe'Int'Unds'",
     ["SortInt"; "SortInt"; "SortInt"])  (* klabel{}("_|Int_"), hook{}("INT.or") *)
  ; ("Lbl'UndsPipe'Set'UndsUnds'SET'Unds'Set'Unds'Set'Unds'Set",
     ["SortSet"; "SortSet"; "SortSet"]) (* hook{}("SET.union") *)
  ; ("LblabsInt'LParUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"]) (* klabel{}("absInt"), hook{}("INT.abs") *)
  ; ("LblbitRangeInt'LParUndsCommUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"; "SortInt"; "SortInt"]) (* klabel{}("bitRangeInt"), hook{}("INT.bitRange") *)
  ; ("LblcategoryChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'String",
     ["SortString"; "SortString"]) (* klabel{}("categoryChar"), hook{}("STRING.category") *)
  ; ("Lblchoice'LParUndsRParUnds'MAP'Unds'KItem'Unds'Map",
     ["SortMap"; "SortKItem"]) (* klabel{}("Map:choice"), hook{}("MAP.choice") *)
  ; ("Lblchoice'LParUndsRParUnds'SET'Unds'KItem'Unds'Set",
     ["SortSet"; "SortKItem"]) (* klabel{}("Set:choice"), hook{}("SET.choice") *)
  ; ("LblchrChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'Int",
     ["SortInt"; "SortString"]) (* klabel{}("chrChar"), hook{}("STRING.chr") *)
  ; ("LblcountAllOccurrences'LParUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortInt"]) (* hook{}("STRING.countAllOccurrences") *)
  ; ("LbldirectionalityChar'LParUndsRParUnds'STRING-COMMON'Unds'String'Unds'String",
     ["SortString"; "SortString"]) (* klabel{}("directionalityChar"), hook{}("STRING.directionality") *)
  ; ("LblfillList'LParUndsCommUndsCommUndsCommUndsRParUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'Int'Unds'KItem",
     ["SortList"; "SortInt"; "SortInt"; "SortKItem"; "SortList"]) (* klabel{}("fillList"), hook{}("LIST.fill") *)
  ; ("LblfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int",
     ["SortString"; "SortString"; "SortInt"; "SortInt"])  (* klabel{}("findChar"), hook{}("STRING.findChar") *)
  ; ("LblfindString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int",
     ["SortString"; "SortString"; "SortInt"; "SortInt"]) (* klabel{}("findString"), hook{}("STRING.find") *)
  ; ("LblintersectSet'LParUndsCommUndsRParUnds'SET'Unds'Set'Unds'Set'Unds'Set",
     ["SortSet"; "SortSet"; "SortSet"]) (* klabel{}("intersectSet"), hook{}("SET.intersection") *)
  ; ("Lblkeys'LParUndsRParUnds'MAP'Unds'Set'Unds'Map",
     ["SortMap"; "SortSet"]) (* klabel{}("keys"), hook{}("MAP.keys") *)
  ; ("Lblkeys'Unds'list'LParUndsRParUnds'MAP'Unds'List'Unds'Map",
     ["SortMap"; "SortList"]) (* hook{}("MAP.keys_list") *)
  ; ("LbllengthString'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String",
     ["SortString"; "SortInt"]) (* klabel{}("lengthString"), hook{}("STRING.length") *)
  ; ("Lbllog2Int'LParUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"]) (* klabel{}("log2Int"), hook{}("INT.log2") *)
  ; ("LblmakeList'LParUndsCommUndsRParUnds'LIST'Unds'List'Unds'Int'Unds'KItem",
     ["SortInt"; "SortKItem"; "SortList"]) (* klabel{}("makeList"), hook{}("LIST.make") *)
  ; ("LblmaxInt'LParUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"; "SortInt"]) (* hook{}("INT.max") *)
  ; ("LblminInt'LParUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"; "SortInt"]) (* hook{}("INT.min") *)
  ; ("LblnewUUID'Unds'STRING-COMMON'Unds'String",
     ["SortString"]) (* hook{}("STRING.uuid") *)
  ; ("LblnotBool'Unds'",
     ["SortBool"; "SortBool"]) (* klabel{}("notBool_"), hook{}("BOOL.not") *)
  ; ("LblordChar'LParUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String",
     ["SortString"; "SortInt"]) (* klabel{}("ordChar"), hook{}("STRING.ord") *)
  ; ("LblrandInt'LParUndsRParUnds'INT'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"])  (* klabel{}("randInt"), hook{}("INT.rand") *)
  ; ("LblremoveAll'LParUndsCommUndsRParUnds'MAP'Unds'Map'Unds'Map'Unds'Set",
       ["SortMap"; "SortSet"; "SortMap"])  (* klabel{}("removeAll"), hook{}("MAP.removeAll") *)
  ; ("Lblreplace'LParUndsCommUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String'Unds'Int",
       ["SortString"; "SortString"; "SortString"; "SortInt"; "SortString"]) (* hook{}("STRING.replace") *)
  ; ("LblreplaceAll'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortString"; "SortString"])  (* hook{}("STRING.replaceAll") *)
  ; ("LblreplaceFirst'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'String'Unds'String",
     ["SortString"; "SortString"; "SortString"; "SortString"]) (* hook{}("STRING.replaceFirst") *)
  ; ("LblrfindChar'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int",
     ["SortString"; "SortString"; "SortInt"; "SortInt"]) (* klabel{}("rfindChar"), hook{}("STRING.rfindChar") *)
  ; ("LblrfindString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'Int'Unds'String'Unds'String'Unds'Int",
     ["SortString"; "SortString"; "SortInt"; "SortInt"])  (* klabel{}("rfindString"), hook{}("STRING.rfind") *)
  ; ("LblsignExtendBitRangeInt'LParUndsCommUndsCommUndsRParUnds'INT-COMMON'Unds'Int'Unds'Int'Unds'Int'Unds'Int",
     ["SortInt"; "SortInt"; "SortInt"; "SortInt"]) (* klabel{}("signExtendBitRangeInt"), hook{}("INT.signExtendBitRange") *)
  ; ("Lblsize'LParUndsRParUnds'LIST'Unds'Int'Unds'List",
     ["SortList"; "SortInt"]) (* klabel{}("sizeList"), hook{}("LIST.size") *)
  ; ("Lblsize'LParUndsRParUnds'MAP'Unds'Int'Unds'Map",
     ["SortMap"; "SortInt"]) (* klabel{}("sizeMap"), hook{}("MAP.size") *)
  ; ("Lblsize'LParUndsRParUnds'SET'Unds'Int'Unds'Set",
     ["SortSet"; "SortInt"]) (* klabel{}("size"), hook{}("SET.size") *)
  ; ("LblsrandInt'LParUndsRParUnds'INT'Unds'K'Unds'Int",
     ["SortInt"; "SortK"]) (* klabel{}("srandInt"), hook{}("INT.srand") *)
  ; ("LblsubstrString'LParUndsCommUndsCommUndsRParUnds'STRING-COMMON'Unds'String'Unds'String'Unds'Int'Unds'Int",
     ["SortString"; "SortInt"; "SortInt"; "SortString"])  (* klabel{}("substrString"), hook{}("STRING.substr") *)
  ; ("LblupdateList'LParUndsCommUndsCommUndsRParUnds'LIST'Unds'List'Unds'List'Unds'Int'Unds'List",
     ["SortList"; "SortInt"; "SortList"; "SortList"]) (* klabel{}("updateList"), hook{}("LIST.updateAll") *)
  ; ("LblupdateMap'LParUndsCommUndsRParUnds'MAP'Unds'Map'Unds'Map'Unds'Map",
     ["SortMap"; "SortMap"; "SortMap"]) (* klabel{}("updateMap"), hook{}("MAP.updateAll") *)
  ; ("Lblvalues'LParUndsRParUnds'MAP'Unds'List'Unds'Map",
       ["SortMap"; "SortList"]) (* klabel{}("values"), hook{}("MAP.values") *)
  ; ("Lbl'Tild'Int'Unds'",
     ["SortInt"; "SortInt"]) (* klabel{}("~Int_"), hook{}("INT.not") *)
]
(*
let special_sym_table =
  [ ]

let ggg : kmodule -> unit = fun m ->
  let name, _, kommand_l, _ = m in
  match name with
  | "BASIC-K" ->
  | "KSEQ"    ->
  | "INJ"     ->
  | "K"       ->
  | s -> if s = semantics_module_name then

         else
           failwith "More than 5 modules"
           *)
(* A transfomer en map *)


(* let appl_patt sym v1 v2 =
   create_appl (create_appl (create_ident sym) (create_pattern v1))
   (create_appl (create_ident sym) (create_pattern v2)) *)



let semantic_rule () =
  let _LT_INT_ = Interface.Output.pp "Lbl'Unds-LT-'Int'Unds'" in
  let _GE_INT_ = Interface.Output.pp "Lbl'Unds-GT-Eqls'Int'Unds'" in
  (* //symbol Lbl'Unds-LT-'Int'Unds' : δ SortInt → δ SortInt → δ SortBool;
        rule Lbl'Unds-LT-'Int'Unds'      0         0     ↪ false
        with Lbl'Unds-LT-'Int'Unds'      0     (succ _)  ↪ true
        with Lbl'Unds-LT-'Int'Unds'  (succ $m) (succ $n) ↪ Lbl'Unds-LT-'Int'Unds'  $m $n
        with Lbl'Unds-LT-'Int'Unds'  (succ _)      0     ↪ false;
        //symbol geb_nat : δ SortInt → injK SortInt → injK SortBool;
        rule Lbl'Unds-GT-Eqls'Int'Unds'     0         0     ↪ true
        with Lbl'Unds-GT-Eqls'Int'Unds'     0     (succ _)  ↪ false
        with Lbl'Unds-GT-Eqls'Int'Unds' (succ $m) (succ $n) ↪ Lbl'Unds-GT-Eqls'Int'Unds' $m $n
        with Lbl'Unds-GT-Eqls'Int'Unds' (succ _)      0     ↪ true; *)
  [ (create_ident _LT_INT_, [create_ident "0" ; create_ident "0"]), (create_ident "false", [])
  ; (create_ident _LT_INT_, [create_ident "0" ; create_appl (create_ident "succ") p_WILD]), (create_ident "true", [])
  ; (create_ident _LT_INT_, [create_one_arg "succ" "m" ; create_one_arg "succ" "n"]),
    (create_ident _LT_INT_, [create_pattern_var "m" ; create_pattern_var "n"])
  ; (create_ident _LT_INT_, [create_appl (create_ident "succ") p_WILD ; create_ident "0"]), (create_ident "false", [])
  ; (create_ident _GE_INT_, [create_ident "0" ; create_ident "0"]), (create_ident "true", [])
  ; (create_ident _GE_INT_, [create_ident "0" ; create_appl (create_ident "succ") p_WILD]), (create_ident "false", [])
  ; (create_ident _GE_INT_, [create_one_arg "succ" "m" ; create_one_arg "succ" "n"]),
    (create_ident _GE_INT_, [create_pattern_var "m" ; create_pattern_var "n"])
  ; (create_ident _GE_INT_, [create_appl (create_ident "succ") p_WILD ; create_ident "0"]), (create_ident "true", []) ]


let create_symbol : string -> p_term -> p_symbol = fun name typ ->
  create_p_symbol [] name [] (Some typ) None

(** [wrap s] creates the p_term δ [s]. *)
let wrap : string -> p_term = fun s -> create_appl p_INJD (create_ident s)

open Printer
open Common.Count_data

(* let print_comment : output -> string -> unit = fun ppf message -> *)
let print_comment ppf message =
  printing ppf "\n// " ; printing ppf message ; printing ppf "\n"

let pp_symbol_prelude : output -> count_data -> printer -> p_symbol -> unit =
  fun ppf cd prt sym ->
  incr_real_symbol cd ;
  (match sym.p_sym_typ with
   | Some v ->
      Translation.Translate.signature :=
        Translation.Axiom.StrMap.add sym.p_sym_nam.elt v !Translation.Translate.signature
   | None -> ()) ;
  prt ppf (no_pos (P_symbol sym))

let pp_builtin_prelude : output -> count_data -> printer -> p_command -> unit =
  fun ppf _ prt b -> prt ppf b

let pp_rule_prelude : output -> count_data -> printer -> p_rule -> unit =
  fun ppf _ prt r -> prt ppf (no_pos (P_rules [r]))

let create_prelude : output -> printer -> string -> unit =
  fun ppf prt _ ->
  let cd = Common.Count_data.reset_count_data 0 in
  let pp = pp_symbol_prelude ppf cd prt in
  let pp_b = pp_builtin_prelude ppf cd prt in
  let pp_r = pp_rule_prelude ppf cd prt in
  (* STEP 1: The injection _INJD: injective symbol δ : SortK → TYPE; *)
  print_comment ppf "Our injection between K and Dedukti";
  pp (create_p_symbol [no_pos (P_prop Injec)] "δ" []
        (Some (create_arrow p_SORTK p_TYPE)) None) ;
  (* Hooked-sort *)
  print_comment ppf "Translation of hooked sorts";
  List.iter (fun n -> pp (create_symbol n p_SORTK)) hooked_sort ;
  (* STEP 2: Some constructors and builtin *)
     print_comment ppf "Some builtins for Lambdapi and constructors";
     (* For inductive type *)
     (* symbol Prop : TYPE; *)
     pp (create_symbol "Prop" p_TYPE) ;
     (* symbol P : Prop → TYPE; *)
     pp (create_symbol "P" (create_arrow (create_ident "Prop") p_TYPE)) ;
     (* builtin "Prop" ≔ Prop; *)
     pp_b (create_builtin_command "Prop" ([], "Prop")) ;
     (* builtin "P" ≔ P; *)
     pp_b (create_builtin_command "P" ([], "P")) ;
     printing ppf "\n";
     (* symbol true : injK SortBool; *)
     pp (create_symbol "true" (wrap "SortBool")) ;
     (* symbol false : injK SortBool; *)
     pp (create_symbol "false" (wrap "SortBool")) ;
     (* constant symbol zero : injK SortInt; *)
     pp (create_symbol "zero" (wrap "SortInt")) ;
     (* constant symbol succ : injK SortInt → injK SortInt; *)
     pp (create_symbol "succ" (create_arrow (wrap "SortInt") (wrap "SortInt"))) ;
     printing ppf "\n";
     (* builtin "0"  ≔ zero; *)
     pp_b (create_builtin_command "0" ([], "zero")) ;
     (* builtin "+1" ≔ succ; *)
     pp_b (create_builtin_command "+1" ([], "succ")) ;
     (* STEP 3: Hooked-symbol *)
     print_comment ppf "Translation of hooked symbols";
     let create_type : string * string list -> p_term = fun (name, type_l) ->
       let rec split_last_value l acc = match l with
         | []  -> failwith ("The symbol " ^ name ^ " has no type.")
         | [t]  -> List.rev acc, t
         | t::q -> split_last_value q (t::acc)
       in
       let head, last = split_last_value type_l [] in
       let f t1 t2 = create_arrow (Translation.Symbol.get_type t1) t2 in
       List.fold_right f head (Translation.Symbol.get_type last)
     in
     List.iter (fun (n,l) -> pp (create_symbol n (create_type (n,l)))) hooked_symbol ;
     (* STEP 4: Add semantic rules *)
     print_comment ppf "Translation of semantic rules";
     List.iter (fun ((hl, bl), (hr, br)) -> pp_r (no_pos (List.fold_left create_appl hl bl, List.fold_left create_appl hr br))) (semantic_rule())
