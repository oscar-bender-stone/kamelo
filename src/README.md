The KaMeLo source file hierarchy  
================================  
 
   src/  
   ├── common/  
   │   ├── color.ml: *Some colors for printing in the terminal*  
   │   ├── error.ml: *Some error messages and execeptions*  
   │   ├── getter.ml: *Some functions on abstract Kore file*  
   │   ├── type.ml: *Type to abstract Kore file*  
   │   └── xlib_OCaml.ml: *Extension of the OCaml standard library*  
   ├── controller/  
   │   ├── old.ml: *The first translation (use the option --old)*  
   │   ├── prelude.ml: *To print the prelude*  
   │   └── with_Viry_encoding.ml: *A translation with Viry encoding*  
   ├── interface/  
   │   ├── getter_term.ml: *Some getters on K and Dedukti*  
   │   ├── K_prelude.ml: *Some specific term of K*  
   │   ├── LP_p_term.ml: *Interface with Lambdapi or Dedukti*  
   │   ├── output.ml: **This file need to move in printing/**  
   │   └── signature.ml: *Data-structure to collect data during the translation*  
   ├── LP/: **This folder MUST BE DELETED**  
   │   ├── LP_printer.ml  
   │   ├── pos.ml  
   │   └── syntax.ml  
   ├── mecanism/: *The main structure of the translation*  
   │   ├── axiom_iterator.ml: *To iterate over an axiom*  
   │   ├── count_data.ml: *Data structure to recap the translation in the terminal*  
   │   └── kommand_iterator.ml: *To iterate over Kore commands*  
   ├── parsing/: *To parse Kore file*  
   │   ├── klexer.mll  
   │   └── kparser.mly  
   ├── printing/  
   │   ├── Kore_printer.ml: *To print into a simplified Kore syntax*  
   │   ├── meta_printer.ml: *Meta-printers to print the resulting translation*  
   │   └── rewrite.py: *Run this script to rewrite a executable program*  
   ├── terminal/: *Management of the terminal*  
   │   ├── cmd_line.ml: *To parse the command line*  
   │   └── display_console.ml: *To print the recap of the translation in the terminal*  
   └── translating/: *The translation of...*  
        ├── axiom.ml: *... the axioms*  
        ├── cleaning: *To clean before translating*  
        ├── eval_strategy.ml: *... the evaluation strategies*  
        ├── executable.ml: *... the executables*  
        ├── import.ml: *... file importation*  
        ├── prelude_data.ml: *Data for translating the prelude*  
        ├── sort.ml: *... the sorts*  
        ├── symbol.ml: *... the symbols*  
        └── viry.ml: *... the conditional rewriting rules*  
 

Note: The files "dune" are just here to compile the OCaml code.  
