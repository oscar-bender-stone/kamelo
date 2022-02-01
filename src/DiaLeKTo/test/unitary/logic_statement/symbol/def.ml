
let pr = cr_parsing_rule v_prefix (cr_prec 42) in
let td = cr_type_decl [] "TYPE" in
let sym = cr_symbol pr v_public v_definable_free "nat" td in
Presilo.Dk.pp_dk_sym_decl (fun i -> i) ppc sym
