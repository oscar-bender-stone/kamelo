module PRIORITY-SYNTAX
   imports INT
   syntax AExp ::= Int | "(" AExp ")" [bracket] | AExp "-" AExp | AExp "+" AExp

endmodule

module PRIORITY
   imports PRIORITY-SYNTAX
   configuration <k> $PGM:AExp </k>

   // syntax KResult ::= Int ne change rien

   rule X - X => 0 [anywhere]
   rule X + 0 => X [priority(60)]
   rule 0 + X => X [priority(42)]
   rule X + Y => X +Int Y [owise]

   // rule ( X:AExp ) => X // WARNING!
endmodule
