require open sem_root.imp-dico;
symbol PGM ≔ <generatedTop> 
          (<T> 
            (<k> 
              (kseq (inj 
                  (decl_SCln__ 
                    (_Comm__ (var_ 0) 
                      (_Comm__ (var_ 1) StopListLBra_Comm__)) 
                    (___ 
                      (___ 
                        (_=_SCln_ (var_ 0) (inj 10)) 
                        (_=_SCln_ (var_ 1) (inj 0))) 
                      (while_do__ 
                        (_<__ (inj 0) (inj (var_ 0))) 
                        (LBra_RBra_ 
                          (___ 
                            (_=_SCln_ (var_ 1) 
                              (_Plus__ (inj (var_ 1)) (inj (var_ 0)))) 
                            (_=_SCln_ (var_ 0) 
                              (_-__ (inj (var_ 0)) (inj 1))))))))) dotk)) 
            (<env> StopDico_DICO_Dico)) 
          (<generatedCounter> 0);
symbol RES ≔ <generatedTop> 
          (<T> 
            (<k> dotk) 
            (<env> 
              (LPar_Pipe-->_RParComm__DICO_Dico_KItem_KItem_Dico (inj (var_ 1)) (inj 55) 
                (LPar_Pipe-->_RParComm__DICO_Dico_KItem_KItem_Dico (inj (var_ 0)) (inj 0) StopDico_DICO_Dico)))) 
          (<generatedCounter> 0);
assert ⊢ PGM ≡ RES;
