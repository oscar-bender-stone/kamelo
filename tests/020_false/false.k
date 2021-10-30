
module FALSE-SYNTAX
   imports BOOL-SYNTAX
   imports INT-SYNTAX

   syntax Bool ::= "f" Int [function, functional]

   syntax Prog ::= "OK" [constructor] | "p" Int [function, functional]

endmodule


module FALSE
   imports FALSE-SYNTAX
   imports BOOL
   imports INT

   rule f 0 => false
   rule f _ => true [owise]

   rule p X => OK requires f X

endmodule
