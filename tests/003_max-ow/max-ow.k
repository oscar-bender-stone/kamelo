
module MAX-OW-SYNTAX
  imports INT
  imports BOOL
  syntax Exp ::= Int | "(" Exp ")" [bracket]
               | "max" Exp Exp

endmodule

module MAX-OW
  imports MAX-OW-SYNTAX
  syntax KResult ::= Int

  rule max X Y => Y requires X <Int Y
  rule max X _ => X [owise]

endmodule
