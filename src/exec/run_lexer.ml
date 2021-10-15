# 1 "run_lexer.mll"
 
  type token =
  | EOF
  | IDENT of string
  | STRING of string

  | L_CURLY_BRA
  | R_CURLY_BRA
  | L_SQUARE_BRA
  | R_SQUARE_BRA
  | L_PAREN
  | R_PAREN
  | COLON
  | COMMA
  | DEF

  | EQUALS
  | EXISTS
  | AND
  | OR
  | NOT
  | IMPLIES
  | BOTTOM
  | TOP
  | REWRITES
  | IN
  | DOM_VAL

  | LOCATION
  | SOURCE
  | PRODUCTION

(** [locate loc] converts the pair of position [loc] of the Lexing library
    into a quadruplet (start_line, start_col, end_line, end_col). *)
let locate : Lexing.position * Lexing.position -> int * int * int * int =
  fun (p1, p2) ->
  let start_line = p1.pos_lnum in
  let start_col = p1.pos_cnum - p1.pos_bol in
  let end_line = p2.pos_lnum in
  let end_col = p2.pos_cnum - p2.pos_bol in
  (start_line, start_col, end_line, end_col)

let unexpected_char : Lexing.lexbuf -> char -> token = fun lexbuf c ->
  let sl, sc, el, ec = locate (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
  LP_interface.Pos.fatal None
     "Unexpected characters [%c] between %i:%i to %i:%i." c sl sc el ec

exception SyntaxError

exception EOFError of string


# 55 "run_lexer.ml"

let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last =
  if lexbuf.Lexing.lex_eof_reached then
    256, _buf, _len, _curr, _last
  else begin
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    lexbuf.Lexing.refill_buff lexbuf;
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = lexbuf.Lexing.lex_last_pos in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    if _curr < _len then
      Char.code (Bytes.unsafe_get _buf _curr), _buf, _len, (_curr + 1), _last
    else
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
  end

let rec __ocaml_lex_state2 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 27 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'@'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
    |39|45|48|49|50|51|52|53|54|55|56|57|64|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      27 (* = last_action *)
  end

and __ocaml_lex_state67 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 27 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'@' *)
    | 64 ->
      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'L' *)
    | 76 ->
      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'P' *)
    | 80 ->
      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'S' *)
    | 83 ->
      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      27 (* = last_action *)
  end

and __ocaml_lex_state68 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 27 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'@' *)
    | 64 ->
      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'L' *)
    | 76 ->
      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'P' *)
    | 80 ->
      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'S' *)
    | 83 ->
      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'r' *)
    | 114 ->
      (* *)
      let _last = _curr in
      (* let _last_action = 27 in*)
      let next_char, _buf, _len, _curr, _last =
        if _curr >= _len then
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
        else
          Char.code (Bytes.unsafe_get _buf _curr),
          _buf, _len, (_curr + 1), _last
      in
      begin match next_char with
        (* |'@' *)
        | 64 ->
          __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'o' *)
        | 111 ->
          (* *)
          let _last = _curr in
          (* let _last_action = 27 in*)
          let next_char, _buf, _len, _curr, _last =
            if _curr >= _len then
              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
            else
              Char.code (Bytes.unsafe_get _buf _curr),
              _buf, _len, (_curr + 1), _last
          in
          begin match next_char with
            (* |'@' *)
            | 64 ->
              __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'L' *)
            | 76 ->
              __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'P' *)
            | 80 ->
              __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'S' *)
            | 83 ->
              __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
            |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
              __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'d' *)
            | 100 ->
              (* *)
              let _last = _curr in
              (* let _last_action = 27 in*)
              let next_char, _buf, _len, _curr, _last =
                if _curr >= _len then
                  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                else
                  Char.code (Bytes.unsafe_get _buf _curr),
                  _buf, _len, (_curr + 1), _last
              in
              begin match next_char with
                (* |'@' *)
                | 64 ->
                  __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'L' *)
                | 76 ->
                  __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'u' *)
                | 117 ->
                  (* *)
                  let _last = _curr in
                  (* let _last_action = 27 in*)
                  let next_char, _buf, _len, _curr, _last =
                    if _curr >= _len then
                      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                    else
                      Char.code (Bytes.unsafe_get _buf _curr),
                      _buf, _len, (_curr + 1), _last
                  in
                  begin match next_char with
                    (* |'@' *)
                    | 64 ->
                      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'L' *)
                    | 76 ->
                      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'P' *)
                    | 80 ->
                      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'c' *)
                    | 99 ->
                      (* *)
                      let _last = _curr in
                      (* let _last_action = 27 in*)
                      let next_char, _buf, _len, _curr, _last =
                        if _curr >= _len then
                          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                        else
                          Char.code (Bytes.unsafe_get _buf _curr),
                          _buf, _len, (_curr + 1), _last
                      in
                      begin match next_char with
                        (* |'@' *)
                        | 64 ->
                          __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'L' *)
                        | 76 ->
                          __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'t' *)
                        | 116 ->
                          (* *)
                          let _last = _curr in
                          (* let _last_action = 27 in*)
                          let next_char, _buf, _len, _curr, _last =
                            if _curr >= _len then
                              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                            else
                              Char.code (Bytes.unsafe_get _buf _curr),
                              _buf, _len, (_curr + 1), _last
                          in
                          begin match next_char with
                            (* |'@' *)
                            | 64 ->
                              __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'L' *)
                            | 76 ->
                              __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'i' *)
                            | 105 ->
                              (* *)
                              let _last = _curr in
                              (* let _last_action = 27 in*)
                              let next_char, _buf, _len, _curr, _last =
                                if _curr >= _len then
                                  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                else
                                  Char.code (Bytes.unsafe_get _buf _curr),
                                  _buf, _len, (_curr + 1), _last
                              in
                              begin match next_char with
                                (* |'o' *)
                                | 111 ->
                                  (* *)
                                  let _last = _curr in
                                  (* let _last_action = 27 in*)
                                  let next_char, _buf, _len, _curr, _last =
                                    if _curr >= _len then
                                      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                    else
                                      Char.code (Bytes.unsafe_get _buf _curr),
                                      _buf, _len, (_curr + 1), _last
                                  in
                                  begin match next_char with
                                    (* |'@' *)
                                    | 64 ->
                                      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                    (* |'L' *)
                                    | 76 ->
                                      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                    (* |'P' *)
                                    | 80 ->
                                      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                    (* |'S' *)
                                    | 83 ->
                                      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                                    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|111|112|113|114|115|116|117|118|119|120|121|122 ->
                                      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                    (* |'n' *)
                                    | 110 ->
                                      (* *)
                                      let _last = _curr in
                                      let _last_action = 25 in
                                      let next_char, _buf, _len, _curr, _last =
                                        if _curr >= _len then
                                          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                        else
                                          Char.code (Bytes.unsafe_get _buf _curr),
                                          _buf, _len, (_curr + 1), _last
                                      in
                                      begin match next_char with
                                        (* |'@' *)
                                        | 64 ->
                                          __ocaml_lex_state2 lexbuf 25 (* = last_action *) _buf _len _curr _last
                                        (* |'L' *)
                                        | 76 ->
                                          __ocaml_lex_state70 lexbuf 25 (* = last_action *) _buf _len _curr _last
                                        (* |'P' *)
                                        | 80 ->
                                          __ocaml_lex_state68 lexbuf 25 (* = last_action *) _buf _len _curr _last
                                        (* |'S' *)
                                        | 83 ->
                                          __ocaml_lex_state69 lexbuf 25 (* = last_action *) _buf _len _curr _last
                                        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                                        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                                          __ocaml_lex_state67 lexbuf 25 (* = last_action *) _buf _len _curr _last
                                        | _ ->
                                          let _curr = _last in
                                          lexbuf.Lexing.lex_curr_pos <- _curr;
                                          lexbuf.Lexing.lex_last_pos <- _last;
                                          25 (* = last_action *)
                                      end
                                    | _ ->
                                      let _curr = _last in
                                      lexbuf.Lexing.lex_curr_pos <- _curr;
                                      lexbuf.Lexing.lex_last_pos <- _last;
                                      27 (* = last_action *)
                                  end
                                (* |'@' *)
                                | 64 ->
                                  __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                (* |'L' *)
                                | 76 ->
                                  __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                (* |'P' *)
                                | 80 ->
                                  __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                (* |'S' *)
                                | 83 ->
                                  __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                                |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
                                  __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                                | _ ->
                                  let _curr = _last in
                                  lexbuf.Lexing.lex_curr_pos <- _curr;
                                  lexbuf.Lexing.lex_last_pos <- _last;
                                  27 (* = last_action *)
                              end
                            (* |'P' *)
                            | 80 ->
                              __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'S' *)
                            | 83 ->
                              __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                            |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                              __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            | _ ->
                              let _curr = _last in
                              lexbuf.Lexing.lex_curr_pos <- _curr;
                              lexbuf.Lexing.lex_last_pos <- _last;
                              27 (* = last_action *)
                          end
                        (* |'P' *)
                        | 80 ->
                          __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'S' *)
                        | 83 ->
                          __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'u'|'v'|'w'|'x'|'y'|'z' *)
                        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|117|118|119|120|121|122 ->
                          __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        | _ ->
                          let _curr = _last in
                          lexbuf.Lexing.lex_curr_pos <- _curr;
                          lexbuf.Lexing.lex_last_pos <- _last;
                          27 (* = last_action *)
                      end
                    (* |'S' *)
                    | 83 ->
                      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    | _ ->
                      let _curr = _last in
                      lexbuf.Lexing.lex_curr_pos <- _curr;
                      lexbuf.Lexing.lex_last_pos <- _last;
                      27 (* = last_action *)
                  end
                (* |'P' *)
                | 80 ->
                  __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'S' *)
                | 83 ->
                  __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'v'|'w'|'x'|'y'|'z' *)
                |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|118|119|120|121|122 ->
                  __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                | _ ->
                  let _curr = _last in
                  lexbuf.Lexing.lex_curr_pos <- _curr;
                  lexbuf.Lexing.lex_last_pos <- _last;
                  27 (* = last_action *)
              end
            | _ ->
              let _curr = _last in
              lexbuf.Lexing.lex_curr_pos <- _curr;
              lexbuf.Lexing.lex_last_pos <- _last;
              27 (* = last_action *)
          end
        (* |'L' *)
        | 76 ->
          __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'P' *)
        | 80 ->
          __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'S' *)
        | 83 ->
          __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
          __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
        | _ ->
          let _curr = _last in
          lexbuf.Lexing.lex_curr_pos <- _curr;
          lexbuf.Lexing.lex_last_pos <- _last;
          27 (* = last_action *)
      end
    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|115|116|117|118|119|120|121|122 ->
      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      27 (* = last_action *)
  end

and __ocaml_lex_state69 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 27 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'@' *)
    | 64 ->
      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'L' *)
    | 76 ->
      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'P' *)
    | 80 ->
      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'S' *)
    | 83 ->
      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'o' *)
    | 111 ->
      (* *)
      let _last = _curr in
      (* let _last_action = 27 in*)
      let next_char, _buf, _len, _curr, _last =
        if _curr >= _len then
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
        else
          Char.code (Bytes.unsafe_get _buf _curr),
          _buf, _len, (_curr + 1), _last
      in
      begin match next_char with
        (* |'@' *)
        | 64 ->
          __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'u' *)
        | 117 ->
          (* *)
          let _last = _curr in
          (* let _last_action = 27 in*)
          let next_char, _buf, _len, _curr, _last =
            if _curr >= _len then
              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
            else
              Char.code (Bytes.unsafe_get _buf _curr),
              _buf, _len, (_curr + 1), _last
          in
          begin match next_char with
            (* |'@' *)
            | 64 ->
              __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'L' *)
            | 76 ->
              __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'r' *)
            | 114 ->
              (* *)
              let _last = _curr in
              (* let _last_action = 27 in*)
              let next_char, _buf, _len, _curr, _last =
                if _curr >= _len then
                  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                else
                  Char.code (Bytes.unsafe_get _buf _curr),
                  _buf, _len, (_curr + 1), _last
              in
              begin match next_char with
                (* |'@' *)
                | 64 ->
                  __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'L' *)
                | 76 ->
                  __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'P' *)
                | 80 ->
                  __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'S' *)
                | 83 ->
                  __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                  __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'c' *)
                | 99 ->
                  (* *)
                  let _last = _curr in
                  (* let _last_action = 27 in*)
                  let next_char, _buf, _len, _curr, _last =
                    if _curr >= _len then
                      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                    else
                      Char.code (Bytes.unsafe_get _buf _curr),
                      _buf, _len, (_curr + 1), _last
                  in
                  begin match next_char with
                    (* |'@' *)
                    | 64 ->
                      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'L' *)
                    | 76 ->
                      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'e' *)
                    | 101 ->
                      (* *)
                      let _last = _curr in
                      let _last_action = 24 in
                      let next_char, _buf, _len, _curr, _last =
                        if _curr >= _len then
                          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                        else
                          Char.code (Bytes.unsafe_get _buf _curr),
                          _buf, _len, (_curr + 1), _last
                      in
                      begin match next_char with
                        (* |'@' *)
                        | 64 ->
                          __ocaml_lex_state2 lexbuf 24 (* = last_action *) _buf _len _curr _last
                        (* |'L' *)
                        | 76 ->
                          __ocaml_lex_state70 lexbuf 24 (* = last_action *) _buf _len _curr _last
                        (* |'P' *)
                        | 80 ->
                          __ocaml_lex_state68 lexbuf 24 (* = last_action *) _buf _len _curr _last
                        (* |'S' *)
                        | 83 ->
                          __ocaml_lex_state69 lexbuf 24 (* = last_action *) _buf _len _curr _last
                        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                          __ocaml_lex_state67 lexbuf 24 (* = last_action *) _buf _len _curr _last
                        | _ ->
                          let _curr = _last in
                          lexbuf.Lexing.lex_curr_pos <- _curr;
                          lexbuf.Lexing.lex_last_pos <- _last;
                          24 (* = last_action *)
                      end
                    (* |'P' *)
                    | 80 ->
                      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'S' *)
                    | 83 ->
                      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    | _ ->
                      let _curr = _last in
                      lexbuf.Lexing.lex_curr_pos <- _curr;
                      lexbuf.Lexing.lex_last_pos <- _last;
                      27 (* = last_action *)
                  end
                | _ ->
                  let _curr = _last in
                  lexbuf.Lexing.lex_curr_pos <- _curr;
                  lexbuf.Lexing.lex_last_pos <- _last;
                  27 (* = last_action *)
              end
            (* |'P' *)
            | 80 ->
              __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'S' *)
            | 83 ->
              __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
            |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|115|116|117|118|119|120|121|122 ->
              __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
            | _ ->
              let _curr = _last in
              lexbuf.Lexing.lex_curr_pos <- _curr;
              lexbuf.Lexing.lex_last_pos <- _last;
              27 (* = last_action *)
          end
        (* |'L' *)
        | 76 ->
          __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'P' *)
        | 80 ->
          __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'S' *)
        | 83 ->
          __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'v'|'w'|'x'|'y'|'z' *)
        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|118|119|120|121|122 ->
          __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
        | _ ->
          let _curr = _last in
          lexbuf.Lexing.lex_curr_pos <- _curr;
          lexbuf.Lexing.lex_last_pos <- _last;
          27 (* = last_action *)
      end
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      27 (* = last_action *)
  end

and __ocaml_lex_state70 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 27 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'@' *)
    | 64 ->
      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'L' *)
    | 76 ->
      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'o' *)
    | 111 ->
      (* *)
      let _last = _curr in
      (* let _last_action = 27 in*)
      let next_char, _buf, _len, _curr, _last =
        if _curr >= _len then
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
        else
          Char.code (Bytes.unsafe_get _buf _curr),
          _buf, _len, (_curr + 1), _last
      in
      begin match next_char with
        (* |'@' *)
        | 64 ->
          __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'c' *)
        | 99 ->
          (* *)
          let _last = _curr in
          (* let _last_action = 27 in*)
          let next_char, _buf, _len, _curr, _last =
            if _curr >= _len then
              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
            else
              Char.code (Bytes.unsafe_get _buf _curr),
              _buf, _len, (_curr + 1), _last
          in
          begin match next_char with
            (* |'@' *)
            | 64 ->
              __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'L' *)
            | 76 ->
              __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'P' *)
            | 80 ->
              __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'S' *)
            | 83 ->
              __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
            |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
              __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
            (* |'a' *)
            | 97 ->
              (* *)
              let _last = _curr in
              (* let _last_action = 27 in*)
              let next_char, _buf, _len, _curr, _last =
                if _curr >= _len then
                  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                else
                  Char.code (Bytes.unsafe_get _buf _curr),
                  _buf, _len, (_curr + 1), _last
              in
              begin match next_char with
                (* |'@' *)
                | 64 ->
                  __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'L' *)
                | 76 ->
                  __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'t' *)
                | 116 ->
                  (* *)
                  let _last = _curr in
                  (* let _last_action = 27 in*)
                  let next_char, _buf, _len, _curr, _last =
                    if _curr >= _len then
                      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                    else
                      Char.code (Bytes.unsafe_get _buf _curr),
                      _buf, _len, (_curr + 1), _last
                  in
                  begin match next_char with
                    (* |'@' *)
                    | 64 ->
                      __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'L' *)
                    | 76 ->
                      __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'i' *)
                    | 105 ->
                      (* *)
                      let _last = _curr in
                      (* let _last_action = 27 in*)
                      let next_char, _buf, _len, _curr, _last =
                        if _curr >= _len then
                          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                        else
                          Char.code (Bytes.unsafe_get _buf _curr),
                          _buf, _len, (_curr + 1), _last
                      in
                      begin match next_char with
                        (* |'@' *)
                        | 64 ->
                          __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'L' *)
                        | 76 ->
                          __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'o' *)
                        | 111 ->
                          (* *)
                          let _last = _curr in
                          (* let _last_action = 27 in*)
                          let next_char, _buf, _len, _curr, _last =
                            if _curr >= _len then
                              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                            else
                              Char.code (Bytes.unsafe_get _buf _curr),
                              _buf, _len, (_curr + 1), _last
                          in
                          begin match next_char with
                            (* |'@' *)
                            | 64 ->
                              __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'L' *)
                            | 76 ->
                              __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'P' *)
                            | 80 ->
                              __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'S' *)
                            | 83 ->
                              __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                            |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|111|112|113|114|115|116|117|118|119|120|121|122 ->
                              __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                            (* |'n' *)
                            | 110 ->
                              (* *)
                              let _last = _curr in
                              let _last_action = 23 in
                              let next_char, _buf, _len, _curr, _last =
                                if _curr >= _len then
                                  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                else
                                  Char.code (Bytes.unsafe_get _buf _curr),
                                  _buf, _len, (_curr + 1), _last
                              in
                              begin match next_char with
                                (* |'@' *)
                                | 64 ->
                                  __ocaml_lex_state2 lexbuf 23 (* = last_action *) _buf _len _curr _last
                                (* |'L' *)
                                | 76 ->
                                  __ocaml_lex_state70 lexbuf 23 (* = last_action *) _buf _len _curr _last
                                (* |'P' *)
                                | 80 ->
                                  __ocaml_lex_state68 lexbuf 23 (* = last_action *) _buf _len _curr _last
                                (* |'S' *)
                                | 83 ->
                                  __ocaml_lex_state69 lexbuf 23 (* = last_action *) _buf _len _curr _last
                                (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                                |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                                  __ocaml_lex_state67 lexbuf 23 (* = last_action *) _buf _len _curr _last
                                | _ ->
                                  let _curr = _last in
                                  lexbuf.Lexing.lex_curr_pos <- _curr;
                                  lexbuf.Lexing.lex_last_pos <- _last;
                                  23 (* = last_action *)
                              end
                            | _ ->
                              let _curr = _last in
                              lexbuf.Lexing.lex_curr_pos <- _curr;
                              lexbuf.Lexing.lex_last_pos <- _last;
                              27 (* = last_action *)
                          end
                        (* |'P' *)
                        | 80 ->
                          __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'S' *)
                        | 83 ->
                          __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
                          __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                        | _ ->
                          let _curr = _last in
                          lexbuf.Lexing.lex_curr_pos <- _curr;
                          lexbuf.Lexing.lex_last_pos <- _last;
                          27 (* = last_action *)
                      end
                    (* |'P' *)
                    | 80 ->
                      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'S' *)
                    | 83 ->
                      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                    | _ ->
                      let _curr = _last in
                      lexbuf.Lexing.lex_curr_pos <- _curr;
                      lexbuf.Lexing.lex_last_pos <- _last;
                      27 (* = last_action *)
                  end
                (* |'P' *)
                | 80 ->
                  __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'S' *)
                | 83 ->
                  __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
                (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'u'|'v'|'w'|'x'|'y'|'z' *)
                |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|117|118|119|120|121|122 ->
                  __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                | _ ->
                  let _curr = _last in
                  lexbuf.Lexing.lex_curr_pos <- _curr;
                  lexbuf.Lexing.lex_last_pos <- _last;
                  27 (* = last_action *)
              end
            | _ ->
              let _curr = _last in
              lexbuf.Lexing.lex_curr_pos <- _curr;
              lexbuf.Lexing.lex_last_pos <- _last;
              27 (* = last_action *)
          end
        (* |'L' *)
        | 76 ->
          __ocaml_lex_state70 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'P' *)
        | 80 ->
          __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'S' *)
        | 83 ->
          __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
        (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
        |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
          __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
        | _ ->
          let _curr = _last in
          lexbuf.Lexing.lex_curr_pos <- _curr;
          lexbuf.Lexing.lex_last_pos <- _last;
          27 (* = last_action *)
      end
    (* |'P' *)
    | 80 ->
      __ocaml_lex_state68 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'S' *)
    | 83 ->
      __ocaml_lex_state69 lexbuf 27 (* = last_action *) _buf _len _curr _last
    (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
    |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|81|82|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
      __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      27 (* = last_action *)
  end


let rec token lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'@'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
      |39|45|48|49|50|51|52|53|54|55|56|57|64|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|112|113|114|115|116|117|118|119|120|121|122 ->
        __ocaml_lex_state2 lexbuf _last_action _buf _len _curr _last
      (* |'}' *)
      | 125 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        4
      (* |'/' *)
      | 47 ->
        (* *)
        let _last = _curr in
        let _last_action = 28 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'/' *)
          | 47 ->
            (* *)
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            2
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            28 (* = last_action *)
        end
      (* |']' *)
      | 93 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        6
      (* |eof *)
      | 256 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        0
      (* |':' *)
      | 58 ->
        (* *)
        let _last = _curr in
        let _last_action = 9 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'=' *)
          | 61 ->
            (* *)
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            11
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            9 (* = last_action *)
        end
      (* |'\\' *)
      | 92 ->
        (* *)
        let _last = _curr in
        let _last_action = 28 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'t' *)
          | 116 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'o' *)
              | 111 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'p' *)
                  | 112 ->
                    (* *)
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    19
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'o' *)
          | 111 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'r' *)
              | 114 ->
                (* *)
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                15
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'d' *)
          | 100 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'v' *)
              | 118 ->
                (* *)
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                22
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'n' *)
          | 110 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'o' *)
              | 111 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'t' *)
                  | 116 ->
                    (* *)
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    16
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'e' *)
          | 101 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'q' *)
              | 113 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'u' *)
                  | 117 ->
                    let next_char, _buf, _len, _curr, _last =
                      if _curr >= _len then
                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                      else
                        Char.code (Bytes.unsafe_get _buf _curr),
                        _buf, _len, (_curr + 1), _last
                    in
                    begin match next_char with
                      (* |'a' *)
                      | 97 ->
                        let next_char, _buf, _len, _curr, _last =
                          if _curr >= _len then
                            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                          else
                            Char.code (Bytes.unsafe_get _buf _curr),
                            _buf, _len, (_curr + 1), _last
                        in
                        begin match next_char with
                          (* |'l' *)
                          | 108 ->
                            let next_char, _buf, _len, _curr, _last =
                              if _curr >= _len then
                                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                              else
                                Char.code (Bytes.unsafe_get _buf _curr),
                                _buf, _len, (_curr + 1), _last
                            in
                            begin match next_char with
                              (* |'s' *)
                              | 115 ->
                                (* *)
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                12
                              | _ ->
                                let _curr = _last in
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                28 (* = last_action *)
                            end
                          | _ ->
                            let _curr = _last in
                            lexbuf.Lexing.lex_curr_pos <- _curr;
                            lexbuf.Lexing.lex_last_pos <- _last;
                            28 (* = last_action *)
                        end
                      | _ ->
                        let _curr = _last in
                        lexbuf.Lexing.lex_curr_pos <- _curr;
                        lexbuf.Lexing.lex_last_pos <- _last;
                        28 (* = last_action *)
                    end
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              (* |'x' *)
              | 120 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'i' *)
                  | 105 ->
                    let next_char, _buf, _len, _curr, _last =
                      if _curr >= _len then
                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                      else
                        Char.code (Bytes.unsafe_get _buf _curr),
                        _buf, _len, (_curr + 1), _last
                    in
                    begin match next_char with
                      (* |'s' *)
                      | 115 ->
                        let next_char, _buf, _len, _curr, _last =
                          if _curr >= _len then
                            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                          else
                            Char.code (Bytes.unsafe_get _buf _curr),
                            _buf, _len, (_curr + 1), _last
                        in
                        begin match next_char with
                          (* |'t' *)
                          | 116 ->
                            let next_char, _buf, _len, _curr, _last =
                              if _curr >= _len then
                                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                              else
                                Char.code (Bytes.unsafe_get _buf _curr),
                                _buf, _len, (_curr + 1), _last
                            in
                            begin match next_char with
                              (* |'s' *)
                              | 115 ->
                                (* *)
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                13
                              | _ ->
                                let _curr = _last in
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                28 (* = last_action *)
                            end
                          | _ ->
                            let _curr = _last in
                            lexbuf.Lexing.lex_curr_pos <- _curr;
                            lexbuf.Lexing.lex_last_pos <- _last;
                            28 (* = last_action *)
                        end
                      | _ ->
                        let _curr = _last in
                        lexbuf.Lexing.lex_curr_pos <- _curr;
                        lexbuf.Lexing.lex_last_pos <- _last;
                        28 (* = last_action *)
                    end
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'i' *)
          | 105 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'n' *)
              | 110 ->
                (* *)
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                21
              (* |'m' *)
              | 109 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'p' *)
                  | 112 ->
                    let next_char, _buf, _len, _curr, _last =
                      if _curr >= _len then
                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                      else
                        Char.code (Bytes.unsafe_get _buf _curr),
                        _buf, _len, (_curr + 1), _last
                    in
                    begin match next_char with
                      (* |'l' *)
                      | 108 ->
                        let next_char, _buf, _len, _curr, _last =
                          if _curr >= _len then
                            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                          else
                            Char.code (Bytes.unsafe_get _buf _curr),
                            _buf, _len, (_curr + 1), _last
                        in
                        begin match next_char with
                          (* |'i' *)
                          | 105 ->
                            let next_char, _buf, _len, _curr, _last =
                              if _curr >= _len then
                                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                              else
                                Char.code (Bytes.unsafe_get _buf _curr),
                                _buf, _len, (_curr + 1), _last
                            in
                            begin match next_char with
                              (* |'e' *)
                              | 101 ->
                                let next_char, _buf, _len, _curr, _last =
                                  if _curr >= _len then
                                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                  else
                                    Char.code (Bytes.unsafe_get _buf _curr),
                                    _buf, _len, (_curr + 1), _last
                                in
                                begin match next_char with
                                  (* |'s' *)
                                  | 115 ->
                                    (* *)
                                    lexbuf.Lexing.lex_curr_pos <- _curr;
                                    lexbuf.Lexing.lex_last_pos <- _last;
                                    17
                                  | _ ->
                                    let _curr = _last in
                                    lexbuf.Lexing.lex_curr_pos <- _curr;
                                    lexbuf.Lexing.lex_last_pos <- _last;
                                    28 (* = last_action *)
                                end
                              | _ ->
                                let _curr = _last in
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                28 (* = last_action *)
                            end
                          | _ ->
                            let _curr = _last in
                            lexbuf.Lexing.lex_curr_pos <- _curr;
                            lexbuf.Lexing.lex_last_pos <- _last;
                            28 (* = last_action *)
                        end
                      | _ ->
                        let _curr = _last in
                        lexbuf.Lexing.lex_curr_pos <- _curr;
                        lexbuf.Lexing.lex_last_pos <- _last;
                        28 (* = last_action *)
                    end
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'b' *)
          | 98 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'o' *)
              | 111 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'t' *)
                  | 116 ->
                    let next_char, _buf, _len, _curr, _last =
                      if _curr >= _len then
                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                      else
                        Char.code (Bytes.unsafe_get _buf _curr),
                        _buf, _len, (_curr + 1), _last
                    in
                    begin match next_char with
                      (* |'t' *)
                      | 116 ->
                        let next_char, _buf, _len, _curr, _last =
                          if _curr >= _len then
                            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                          else
                            Char.code (Bytes.unsafe_get _buf _curr),
                            _buf, _len, (_curr + 1), _last
                        in
                        begin match next_char with
                          (* |'o' *)
                          | 111 ->
                            let next_char, _buf, _len, _curr, _last =
                              if _curr >= _len then
                                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                              else
                                Char.code (Bytes.unsafe_get _buf _curr),
                                _buf, _len, (_curr + 1), _last
                            in
                            begin match next_char with
                              (* |'m' *)
                              | 109 ->
                                (* *)
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                18
                              | _ ->
                                let _curr = _last in
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                28 (* = last_action *)
                            end
                          | _ ->
                            let _curr = _last in
                            lexbuf.Lexing.lex_curr_pos <- _curr;
                            lexbuf.Lexing.lex_last_pos <- _last;
                            28 (* = last_action *)
                        end
                      | _ ->
                        let _curr = _last in
                        lexbuf.Lexing.lex_curr_pos <- _curr;
                        lexbuf.Lexing.lex_last_pos <- _last;
                        28 (* = last_action *)
                    end
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'a' *)
          | 97 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'n' *)
              | 110 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'d' *)
                  | 100 ->
                    (* *)
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    14
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          (* |'r' *)
          | 114 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'e' *)
              | 101 ->
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'w' *)
                  | 119 ->
                    let next_char, _buf, _len, _curr, _last =
                      if _curr >= _len then
                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                      else
                        Char.code (Bytes.unsafe_get _buf _curr),
                        _buf, _len, (_curr + 1), _last
                    in
                    begin match next_char with
                      (* |'r' *)
                      | 114 ->
                        let next_char, _buf, _len, _curr, _last =
                          if _curr >= _len then
                            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                          else
                            Char.code (Bytes.unsafe_get _buf _curr),
                            _buf, _len, (_curr + 1), _last
                        in
                        begin match next_char with
                          (* |'i' *)
                          | 105 ->
                            let next_char, _buf, _len, _curr, _last =
                              if _curr >= _len then
                                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                              else
                                Char.code (Bytes.unsafe_get _buf _curr),
                                _buf, _len, (_curr + 1), _last
                            in
                            begin match next_char with
                              (* |'t' *)
                              | 116 ->
                                let next_char, _buf, _len, _curr, _last =
                                  if _curr >= _len then
                                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                  else
                                    Char.code (Bytes.unsafe_get _buf _curr),
                                    _buf, _len, (_curr + 1), _last
                                in
                                begin match next_char with
                                  (* |'e' *)
                                  | 101 ->
                                    let next_char, _buf, _len, _curr, _last =
                                      if _curr >= _len then
                                        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                                      else
                                        Char.code (Bytes.unsafe_get _buf _curr),
                                        _buf, _len, (_curr + 1), _last
                                    in
                                    begin match next_char with
                                      (* |'s' *)
                                      | 115 ->
                                        (* *)
                                        lexbuf.Lexing.lex_curr_pos <- _curr;
                                        lexbuf.Lexing.lex_last_pos <- _last;
                                        20
                                      | _ ->
                                        let _curr = _last in
                                        lexbuf.Lexing.lex_curr_pos <- _curr;
                                        lexbuf.Lexing.lex_last_pos <- _last;
                                        28 (* = last_action *)
                                    end
                                  | _ ->
                                    let _curr = _last in
                                    lexbuf.Lexing.lex_curr_pos <- _curr;
                                    lexbuf.Lexing.lex_last_pos <- _last;
                                    28 (* = last_action *)
                                end
                              | _ ->
                                let _curr = _last in
                                lexbuf.Lexing.lex_curr_pos <- _curr;
                                lexbuf.Lexing.lex_last_pos <- _last;
                                28 (* = last_action *)
                            end
                          | _ ->
                            let _curr = _last in
                            lexbuf.Lexing.lex_curr_pos <- _curr;
                            lexbuf.Lexing.lex_last_pos <- _last;
                            28 (* = last_action *)
                        end
                      | _ ->
                        let _curr = _last in
                        lexbuf.Lexing.lex_curr_pos <- _curr;
                        lexbuf.Lexing.lex_last_pos <- _last;
                        28 (* = last_action *)
                    end
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    28 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                28 (* = last_action *)
            end
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            28 (* = last_action *)
        end
      (* |')' *)
      | 41 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        8
      (* |'\t'|'\n'|' ' *)
      |9|10|32 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        1
      (* |'o' *)
      | 111 ->
        (* *)
        let _last = _curr in
        let _last_action = 27 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'@'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
          |39|45|48|49|50|51|52|53|54|55|56|57|64|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|115|116|117|118|119|120|121|122 ->
            __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
          (* |'r' *)
          | 114 ->
            (* *)
            let _last = _curr in
            (* let _last_action = 27 in*)
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'@'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
              |39|45|48|49|50|51|52|53|54|55|56|57|64|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|95|97|98|99|100|101|102|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
              (* |'g' *)
              | 103 ->
                (* *)
                let _last = _curr in
                (* let _last_action = 27 in*)
                let next_char, _buf, _len, _curr, _last =
                  if _curr >= _len then
                    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
                  else
                    Char.code (Bytes.unsafe_get _buf _curr),
                    _buf, _len, (_curr + 1), _last
                in
                begin match next_char with
                  (* |'@' *)
                  | 64 ->
                    __ocaml_lex_state2 lexbuf 27 (* = last_action *) _buf _len _curr _last
                  (* |'\''|'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
                  |39|45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
                    __ocaml_lex_state67 lexbuf 27 (* = last_action *) _buf _len _curr _last
                  | _ ->
                    let _curr = _last in
                    lexbuf.Lexing.lex_curr_pos <- _curr;
                    lexbuf.Lexing.lex_last_pos <- _last;
                    27 (* = last_action *)
                end
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                27 (* = last_action *)
            end
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            27 (* = last_action *)
        end
      (* |'(' *)
      | 40 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        7
      (* |'[' *)
      | 91 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        5
      (* |'{' *)
      | 123 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        3
      (* |',' *)
      | 44 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        10
      (* |'"' *)
      | 34 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        26
      | _ ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        28
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
# 63 "run_lexer.mll"
                         ( EOF            )
# 1810 "run_lexer.ml"

  | 1 ->
# 64 "run_lexer.mll"
                         ( token lexbuf   )
# 1815 "run_lexer.ml"

  | 2 ->
# 65 "run_lexer.mll"
                         ( comment lexbuf )
# 1820 "run_lexer.ml"

  | 3 ->
# 67 "run_lexer.mll"
                         ( L_CURLY_BRA    )
# 1825 "run_lexer.ml"

  | 4 ->
# 68 "run_lexer.mll"
                         ( R_CURLY_BRA    )
# 1830 "run_lexer.ml"

  | 5 ->
# 69 "run_lexer.mll"
                         ( L_SQUARE_BRA   )
# 1835 "run_lexer.ml"

  | 6 ->
# 70 "run_lexer.mll"
                         ( R_SQUARE_BRA   )
# 1840 "run_lexer.ml"

  | 7 ->
# 71 "run_lexer.mll"
                         ( L_PAREN        )
# 1845 "run_lexer.ml"

  | 8 ->
# 72 "run_lexer.mll"
                         ( R_PAREN        )
# 1850 "run_lexer.ml"

  | 9 ->
# 73 "run_lexer.mll"
                         ( COLON          )
# 1855 "run_lexer.ml"

  | 10 ->
# 74 "run_lexer.mll"
                         ( COMMA          )
# 1860 "run_lexer.ml"

  | 11 ->
# 75 "run_lexer.mll"
                         ( DEF            )
# 1865 "run_lexer.ml"

  | 12 ->
# 77 "run_lexer.mll"
                         ( EQUALS         )
# 1870 "run_lexer.ml"

  | 13 ->
# 78 "run_lexer.mll"
                         ( EXISTS         )
# 1875 "run_lexer.ml"

  | 14 ->
# 79 "run_lexer.mll"
                         ( AND            )
# 1880 "run_lexer.ml"

  | 15 ->
# 80 "run_lexer.mll"
                         ( OR             )
# 1885 "run_lexer.ml"

  | 16 ->
# 81 "run_lexer.mll"
                         ( NOT            )
# 1890 "run_lexer.ml"

  | 17 ->
# 82 "run_lexer.mll"
                         ( IMPLIES        )
# 1895 "run_lexer.ml"

  | 18 ->
# 83 "run_lexer.mll"
                         ( BOTTOM         )
# 1900 "run_lexer.ml"

  | 19 ->
# 84 "run_lexer.mll"
                         ( TOP            )
# 1905 "run_lexer.ml"

  | 20 ->
# 85 "run_lexer.mll"
                         ( REWRITES       )
# 1910 "run_lexer.ml"

  | 21 ->
# 86 "run_lexer.mll"
                         ( IN             )
# 1915 "run_lexer.ml"

  | 22 ->
# 87 "run_lexer.mll"
                         ( DOM_VAL        )
# 1920 "run_lexer.ml"

  | 23 ->
# 89 "run_lexer.mll"
                         ( LOCATION       )
# 1925 "run_lexer.ml"

  | 24 ->
# 90 "run_lexer.mll"
                         ( SOURCE         )
# 1930 "run_lexer.ml"

  | 25 ->
# 91 "run_lexer.mll"
                         ( PRODUCTION     )
# 1935 "run_lexer.ml"

  | 26 ->
# 93 "run_lexer.mll"
                    ( quote (Buffer.create 200) lexbuf )
# 1940 "run_lexer.ml"

  | 27 ->
# 94 "run_lexer.mll"
                    ( let yytext = Lexing.lexeme lexbuf in
		              IDENT yytext )
# 1946 "run_lexer.ml"

  | 28 ->
let
# 96 "run_lexer.mll"
         c
# 1952 "run_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 96 "run_lexer.mll"
                    ( unexpected_char lexbuf c  )
# 1956 "run_lexer.ml"

  | _ -> raise (Failure "lexing: empty token")


and comment lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |'\n' *)
      | 10 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        0
      (* |eof *)
      | 256 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        2
      | _ ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        1
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
# 99 "run_lexer.mll"
         ( token   lexbuf                                          )
# 2009 "run_lexer.ml"

  | 1 ->
# 100 "run_lexer.mll"
         ( comment lexbuf                                          )
# 2014 "run_lexer.ml"

  | 2 ->
# 101 "run_lexer.mll"
         ( raise (EOFError "Unexpected end of file in comment.")   )
# 2019 "run_lexer.ml"

  | _ -> raise (Failure "lexing: empty token")


and quote buf lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |'\\' *)
      | 92 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        0
      (* |eof *)
      | 256 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        3
      (* |'"' *)
      | 34 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        1
      | _ ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        2
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
# 104 "run_lexer.mll"
            ( after_backslash buf lexbuf                           )
# 2078 "run_lexer.ml"

  | 1 ->
# 105 "run_lexer.mll"
            ( STRING (Buffer.contents buf)                         )
# 2083 "run_lexer.ml"

  | 2 ->
let
# 106 "run_lexer.mll"
         c
# 2089 "run_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 106 "run_lexer.mll"
            ( Buffer.add_char buf c; quote buf lexbuf              )
# 2093 "run_lexer.ml"

  | 3 ->
# 107 "run_lexer.mll"
            ( raise (EOFError "Unexpected end of file in string.") )
# 2098 "run_lexer.ml"

  | _ -> raise (Failure "lexing: empty token")


and after_backslash buf lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |eof *)
      | 256 ->
        let _curr = _last in
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        _last_action
      (* |'"' *)
      | 34 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        0
      | _ ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        1
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
# 110 "run_lexer.mll"
           ( quote buf lexbuf )
# 2151 "run_lexer.ml"

  | 1 ->
let
# 111 "run_lexer.mll"
         c
# 2157 "run_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 111 "run_lexer.mll"
           ( Buffer.add_char buf '\\'; Buffer.add_char buf c; quote buf lexbuf )
# 2161 "run_lexer.ml"

  | _ -> raise (Failure "lexing: empty token")


;;

