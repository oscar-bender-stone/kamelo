KaMeLo  
========  

KaMeLo is a translator from **the semantical framework K** to **the logical framework Dedukti**.  
More precisely, KaMeLo takes as input **a Kore file**, that is the translation of a K semantics into **a Matching Logic theory**, and generates a Lambdapi file or a Dedukti file.  
More concretly, you can translate a K semantics, let say *imp-dico.k*, into Dedukti as follow:  
  - *kompile imp-dico.k*  
  - *cd kamelo*  
  - *make*  
  - *./KaMeLo -r --lib ../imp-dico-kompiled/definition.kore*  

The option "-r" is used to obtain more readable identifiers.  
The option "--lib" is used to include the manual translation of the K standard library into the generated file.  

Moreover, you can translate the program *sum.imp*, which follows the K semantics *imp-dico.lp*, into Dedukti as follow:  
  - *bash utilities/translate_pgm.sh imp-dico.lp sum.imp*  

Note: Resulting outputs can be found in the folder *example/*.  

Finally, you can run all the tests with *make test-lp*.  
If you interrupt a *kompile* command, you may need to run *test-clean*.  

The files' hierarchy of KaMeLo  
==============================  

.  
├── main.ml: *The main algorithm to translate a semantics or an executable*  
├── Makefile  
├── README.md: *This file*  
├── example: *Examples of K semantics translation*  
├── src: *Source of KaMeLo. (See src/README.md)*  
├── utilities: *Some useful scripts*  
└── tests  
      ├── 001_imp-dico/: *A modified IMP semantics*   
      ├── 002_imp-lib/: *A IMP semantics that uses the K standard library*   
      └── gen_tests.sh: *Run this script to launch all the tests*  

Note: The files "dune" and "dune-project" are just here to compile the OCaml code.  

Paper  
=====  

This translator is described in the following paper:  
A. Ledein, V. Blot, C. Dubois, *Vers une traduction de K en Dedukti*,  
JFLA 2022, Juin 2022, Périgord, France. (See http://jfla.inria.fr/jfla2022.html)  

The following command is used to retrieve the version of the translator KaMeLo  
associated with the previous article:  

   git clone -b JFLA2022 https://gitlab.com/semantiko/kamelo.git
