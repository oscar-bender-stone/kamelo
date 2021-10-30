

module SORTED-SYNTAX
   imports BOOL-SYNTAX
   imports INT-SYNTAX

   syntax L ::= List{Int, ","}

   syntax Bool ::= "sorted" L [function, functional]

   syntax Int ::= "min" L [function, functional]

endmodule


module SORTED
   imports SORTED-SYNTAX
   imports INT

   rule sorted .L => true
   rule sorted _ , .L => true
   rule sorted A1 , A2 , L => (A1 <Int A2) andBool (sorted A2 , L)

   rule min .L => 0
   rule min A1 , L => A1 requires (sorted A1 , L)


endmodule
