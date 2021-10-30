requires "imp.k"
//requires "domains.md"

module SIMPLE-SPEC-SYNTAX
    imports IMP-SYNTAX
endmodule

module VERIFICATION
    imports SIMPLE-SPEC-SYNTAX
    imports IMP

    rule maxInt(X, Y) => Y requires         X <Int Y [simplification]
    rule maxInt(X, Y) => X requires notBool X <Int Y [simplification]
endmodule

module SIMPLE-SPEC
    imports VERIFICATION

    claim <k> 3 + 4 => 7 ... </k>

    claim <k> if 3 < 4 then $c = 1 ; else $c = 2 ;
                 => . ... </k>
          <env> ENV => ENV [ $c <- 1 ] </env>

    // claim <k> $a = A:Int ; $b = B:Int ;
    //           if A < B then $c = B ; else $c = A ;
    //               => . ... </k>
    //       <env> ENV => ENV [ $a <- A ] [ $b <- B ] [ $c <- ?C:Int ] </env>
    //   requires A <=Int ?C
    //    andBool B <=Int ?C

    // claim <k> $a = A:Int ; $b = B:Int ;
    //           if A < B then $c = B ; else $c = A ;
    //                 => . ... </k>
    //       <env> ENV => ENV [ $a <- A ] [ $b <- B ] [ $c <- maxInt(A, B) ] </env>
endmodule
