module MINUS-MINI-SYNTAX
    imports INT
	syntax AExp  ::= Int | "(" AExp ")" [bracket] |
                     AExp "-" AExp    [strict(1), left]
endmodule

module MINUS-MINI
	imports MINUS-MINI-SYNTAX
	syntax KResult ::= Int

	configuration <T color="yellow">
            	   <k color="green"> $PGM:AExp </k>
		            </T>
	rule X  - X  => 0
endmodule
