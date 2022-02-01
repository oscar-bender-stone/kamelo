(* nat : type. *)
cr_symbol v_infix_left v_public v_definable_free

(* 0 : eta nat. *)


(* S : eta (arr nat nat). *)


(* def plus : eta (arr nat (arr nat nat)). *)


(* (; Out of scope of sttfa, this sttfa + rewrite rules. ;) *)
cr_comment "Out of scope of sttfa, this sttfa + rewrite rules."

(* [y] plus 0 y --> y. *)
cr_rule ("plus", ["0", "y"]) ("y", [])

(* [x,y] plus (S x) y --> S (plus x y). *)
cr

(* [x] plus x 0 --> x. *)
cr_rule ("plus", ["x", "0"]) ("x", [])

(* [x,y] plus x (S y) --> S (plus x y). *)
