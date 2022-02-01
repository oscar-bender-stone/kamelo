DiaLeKTo  
========  

This project implements a DSL to generate either Dedukti or Lambdapi.  
In other words, an OCaml developer could use it to generate Dedukti or Lambdapi, in a unified way.

The main goals of DialeKTo are:  
- Highlight what can be done with these languages  
- Better understood of the links with the lambda-pi-calculus modulo theory  
- Simplify writing a translator  
- Allow that translator to generate Dedukti or Lambdapi  

The hierarchy of files is:  

.  
├── common  
│   ├── color.ml: *Definitions of some colors*  
│   └── error.ml: *To print some warnings*  
├── constructor: *Constructors for ...*  
│   ├── command.ml: *... command*  
│   └── term.ml: *... term*  
├── getter: *Getters for ...*  
│   └── command.ml: *... command*  
├── example  
│   ├── nat.ml  
│   └── STTforall.ml  
├── lib  
│   ├── command.ml  
│   └── create.ml  
├── main.ml  
├── Makefile  
├── presilo: *"printer" in esperanto*  
│   ├── common_presilo.ml: *Common functions for the printers*  
│   ├── dk.ml: *The printer of Dedukti*  
│   └── lp.ml: *The printer of Lambdapi*  
├── README.md: *This current file*  
└── type: *The general type ...*  
    ├── command.ml: *... of command*  
    └── term.ml: *... of term*  

Note: The files "dune" and "dune-project" are just here to compile the OCaml code.  


