open Constructor.Command
open Constructor.Term

open Common.Error

let count = ref 1

let () =
  (* let filename = "toto.txt" in
  let f  = open_out filename in
  let ppc = Format.formatter_of_out_channel f in *)

  let ppc = Format.std_formatter in

  let head_DK () =
    print ppc "%s" ("\n\n* ***** * Test " ^
      (string_of_int !count) ^ " - DK: * ***** *\n") in
  let head_LP () =
    print ppc "%s" ("\n* ***** * Test " ^
      (string_of_int !count) ^ " - LP: * ***** *\n") ; incr count in

  let pp_LP = Presilo.Lp.pp_lp_command (fun i -> i) ppc in
  let pp_DK = Presilo.Dk.pp_dk_command (fun i -> i) ppc in

  (* Test symbol - prefix public static *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_public v_static "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test symbol - prefix public free *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_public v_definable_free "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test symbol - infix private free *)
  head_DK () ;
  let pr = cr_parsing_rule v_infix_left (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_private v_definable_free "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test symbol - closed protected free *)
  head_DK () ;
  let pr = cr_parsing_rule v_closed (cr_prec 62) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_protected v_definable_free "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test symbol - prefix public AC *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_public v_definable_AC "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test symbol - prefix public ACU *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let sym = cr_symbol pr v_public (v_definable_ACU (cr_sym "0")) "nat" td in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test definition - prefix public free *)
  let lambda_x_x = (cr_lambda((["x"], None), cr_var "x")) in
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let lt = cr_lambda_term lambda_x_x in
  let sym = cr_definition pr v_public v_definable_free "nat" td lt false in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test definition - prefix public injective *) (* TODO fix ? *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let lt = cr_lambda_term lambda_x_x in
  let sym = cr_definition pr v_public v_injective "nat" td lt false in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test definition - prefix public constant *) (* TODO fix ? *)
  head_DK () ;
  let pr = cr_parsing_rule v_prefix (cr_prec 42) in
  let td = cr_type_decl [] v_TYPE in
  let lt = cr_lambda_term lambda_x_x in
  let sym = cr_definition pr v_public v_static "nat" td lt false in
  pp_DK sym ;

  head_LP () ; pp_LP sym ;

  (* Test query - eval 10 SNF *)
  head_DK () ;
  let e = cr_eval (v_config (Some 10) (Some v_SNF)) v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - eval _ SNF *)
  head_DK () ;
  let e = cr_eval (v_config None (Some v_SNF)) v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - eval 10 _ *)
  head_DK () ;
  let e = cr_eval (v_config (Some 10) None) v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - eval _ _ *)
  head_DK () ;
  let e = cr_eval (v_config None None) v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - infer 42 WHNF *)
  head_DK () ;
  let e = cr_infer (v_config (Some 42) (Some v_WHNF)) v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - check *)
  head_DK () ;
  let e = cr_check v_conversion v_TYPE v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - checknot *)
  head_DK () ;
  let e = cr_check v_conversion_not v_TYPE v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - assert *)
  head_DK () ;
  let e = cr_assert v_has_type v_TYPE v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - assertnot *)
  head_DK () ;
  let e = cr_assert v_has_type_not v_TYPE v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - pp_term *)
  head_DK () ;
  let e = cr_pp_term v_TYPE in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - pp_goal *)
  head_DK () ;
  let e = cr_pp_goal in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - proof term *)
  head_DK () ;
  let e = cr_pp_proof_term in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test query - message *)
  head_DK () ;
  let e = cr_pp_message "Hello world!" in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test import *)
  head_DK () ;
  let e = cr_include ("tic", "tac") in (* TODO fix *)
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test comment *)
  head_DK () ;
  let e = cr_comment "Hello world!" in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* Test builtin *)
  head_DK () ;
  let e = cr_builtin v_Eq "=" in
  pp_DK e ;

  head_LP () ; pp_LP e ;

  (* cr_rule ("plus", ["0", "y"]) ("y", []) ; *)

  Format.pp_print_flush ppc ();
  (* close_out f ; *)
  flush stdout;;
