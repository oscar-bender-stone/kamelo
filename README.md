KaMeLo  
========  

KaMeLo is a translator from **the semantical framework K** to **the logical framework Dedukti**.  
More precisely, KaMeLo takes as input **a Kore file**, that is the translation of a K semantic into **a Matching Logic theory**, and generates a Lambdapi file or a Dedukti file.  
More concretly, you can translate a K semantic, let say *imp-dico.k*, into Dedukti as follow:  
  - *kompile imp-dico.k*  
  - *cd kamelo*  
  - *make*  
  - *./KaMeLo -r ../imp-dico-kompiled/definition.kore*  

The option "-r" is used to obtain more readable identifiers.  

You can run all the tests with *make test-lp*.  
If you interrupt a *kompile* command, you may need to run *test-clean*.  

The files' hierarchy of KaMeLo  
==============================  

.  
├── main.ml: *The main algorithm to translate a semantic or an executable*  
├── Makefile  
├── README.md: *This file*  
├── src: *Source of KaMeLo. (See src/README.md)*  
└── tests  
      ├── 001_imp-dico/: *The modified IMP semantic*   
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
