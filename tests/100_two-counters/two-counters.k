// From the article "Towards a Trustworthy Semantics-Based Language Framework via Proof Generation", CAV 2021

module TWO-COUNTERS-SYNTAX
   imports INT
   syntax State ::= "<" Int "," Int ">"
endmodule

module TWO-COUNTERS
   imports TWO-COUNTERS-SYNTAX
   configuration <k> $PGM:State </k>
   rule <M, N> => <M -Int 1, N +Int M> requires M >Int 0
endmodule
