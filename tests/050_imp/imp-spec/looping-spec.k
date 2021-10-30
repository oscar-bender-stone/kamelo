requires "imp.k"

module VERIFICATION
    imports IMP
endmodule

module LOOPING-SPEC
    imports VERIFICATION

    claim <k> while 0 < $n do {
                $s = $s + $n ;
                $n = $n - 1 ;
              }
           => . ... </k>
          <env> $s |-> (S:Int => S +Int ((N +Int 1) *Int N /Int 2))
                $n |-> (N:Int => 0)
          </env>
      requires N >=Int 0
       andBool S >=Int 0

endmodule

