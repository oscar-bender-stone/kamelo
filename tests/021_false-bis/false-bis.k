
module FALSE-BIS-SYNTAX
   imports BOOL-SYNTAX

   syntax S ::= "a" [constructor] | "b" [constructor]
   syntax Bool ::= "f" S [function, functional]
   syntax Prog ::= "OK" [constructor] | "p" S [function, functional]

endmodule

module FALSE-BIS
   imports FALSE-BIS-SYNTAX

   rule f a => false
   rule f _ => true [owise]

   rule p X => OK requires f X

endmodule
