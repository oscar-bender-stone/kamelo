(* type : Type. *)
cr_symbol_default v_static "type" "Type"

(* arr : type -> type -> type. *)
cr_symbol_default v_static "arr" (cr_arrow ["type", "type", "type"])

(* bool : type. *)
cr_symbol_default v_static "bool" "type"

(* def eta : type -> Type. *)
cr_symbol_default v_definable_free "eta" (cr_arrow ["type", "Type"])

(* ptype : Type. *)
cr_symbol_default v_static "ptype" "Type"

(* p : type -> ptype. *)
cr_symbol_default v_static "p" (cr_arrow ["type", "ptype"])

(* def etap : ptype -> Type. *)
cr_symbol_default v_definable_free "eta" (cr_arrow ["type", "Type"])

(* forallK : (type -> ptype) -> ptype. *)


(* def eps : eta bool -> Type. *)


(* impl : eta bool -> eta bool -> eta bool. *)


(* forall : t:type -> (eta t -> eta bool) -> eta bool. *)


(* forallP : (type -> eta bool) -> eta bool. *)


(* [] eta --> t : type => etap (p t). *)


(* [l,r] etap (p (arr l r)) --> eta l -> eta r. *)


(* [f] etap (forallK f) --> x : type -> etap (f x). *)


(* [t,f] eps (forall t f) --> x:eta t -> eps (f x). *)


(* [l,r] eps (impl l r) --> eps l -> eps r. *)


(* [f] eps (forallP f) --> x:type -> eps (f x). *)
