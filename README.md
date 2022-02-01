KaMeLo  
========  

KaMeLo is a translator from **the semantical framework K** to **the logical framework Dedukti**.  
More precisely, KaMeLo takes as input **a Kore file**, that is the translation of a K semantic into **a Matching Logic theory**, and generates a Lambdapi file or a Dedukti file.  
More concretly, you can translate a K semantic, let say *imp.k*, into Dedukti as follow:  
  - *kompile imp.k*  
  - *cd kamelo*  
  - *make*  
  - *./KaMeLo ../imp-kompiled/definition.kore*  

The hierarchy of files is:  

.  
├── gen_tests.sh: *Run this script to launch all the tests*  
├── main.ml  
├── Makefile  
├── rewrite.py: *Run this script to rewrite a executable program*  
├── sauv/: **This folder MUST BE DELETED**  
└── src/  
   ├── common/  
   │   ├── color.ml: *Some colors for printing in the terminal*  
   │   ├── count_data.ml: *Data structure to recap the translation in the terminal*  
   │   └── type.ml: *Type to abstract Kore file*  
   ├── DiaLeKTo/: *See https://gitlab.com/semantiko/dialekto for more information*  
   │   ├── common/  
   │   ├── constructor/: *Some constructors*  
   │   ├── getter/: *Some getters*  
   │   ├── main.ml  
   │   ├── Makefile  
   │   ├── presilo/: *Printers for Dedukti and Lambdapi*  
   │   ├── README.md: *This file*  
   │   ├── test/  
   │   └── type/: *Main types*  
   ├── interface/  
   │   ├── K_prelude.ml: *Some specific term of K*  
   │   ├── LP_p_term.ml: *Interface with Lambdapi or Dedukti*  
   │   └── output.ml: **This file need to move in printing/**  
   ├── LP/: **This folder MUST BE DELETED**  
   │   ├── LP_printer.ml  
   │   ├── pos.ml  
   │   └── syntax.ml  
   ├── mecanism/: *The main structure of the translation*  
   │   ├── dependency_graph.ml: **This file is OBSOLETE**  
   │   ├── iterator.ml: **This file is OBSOLETE**  
   │   └── iterator_plus_plus.ml  
   ├── parsing/: *To parse Kore file*  
   │   ├── klexer.mll  
   │   └── kparser.mly  
   ├── printing/  
   │   ├── import.ml: *About file management*  
   │   ├── prelude.ml: *To print the prelude*  
   │   └── printer.ml: *To print Lambdapi, Dedukti or Kore syntax*  
   ├── terminal/: *Management of the terminal*  
   │   ├── cmd_line.ml: *To parse the command line*  
   │   ├── display_console.ml: *To print the recap of the translation in the terminal*  
   │   └── preprocessing.ml: **This file is OBSOLETE**  
   └── translation/: *The translation of...*  
       ├── axiom.ml: *... the axioms*  
       ├── symbol.ml: *... the symbols*  
       ├── translate.ml: *... a file*  
       └── viry.ml: *... the conditional rewriting rules*  
 

Note: The files "dune" and "dune-project" are just here to compile the OCaml code.  
