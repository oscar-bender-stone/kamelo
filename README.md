# KaMeLo (Docker)

A docker container to run the [KaMeLo](https://gitlab.com/semantiko/kamelo)
translator, created by Amélie Ledein, Valentin Blot, and Catherine Dubois.
KaMeLo translates specifications written in the
[K Framework](https://kframework.org/) into the logical framework
[Dedukti](https://github.com/Deducteam/Dedukti). Due to breaking changes in K,
the container needs specific package versions. Most of the container is adapted
from the old CI script.

Recommended Docker version: 23.0+.

For an overview of KaMeLo, refer to this paper:

> Amélie Ledein, Valentin Blot, Catherine Dubois. A semantics of K into dedukti.
> TYPES 2022 - 28th International Conference on Types for Proofs and Programs
> (TYPES), Jul 2023, Nantes, France. ⟨10.4230/LIPIcs.TYPES.2022.23⟩.
> ⟨hal-03895834v2⟩

The original README is provided below.

---

KaMeLo is a translator from **the semantical framework K** to **the logical
framework Dedukti**.\
More precisely, KaMeLo takes as input **a Kore file**, that is the translation
of a K semantics into **a Matching Logic theory**, and generates a Lambdapi file
or a Dedukti file.\
More concretly, you can translate a K semantics, let say _imp-dico.k_, into
Dedukti as follow:

- _kompile imp-dico.k_
- _cd kamelo_
- _make_
- _./KaMeLo -r --lib ../imp-dico-kompiled/definition.kore_

The option "-r" is used to obtain more readable identifiers.\
The option "--lib" is used to include the manual translation of the K standard
library into the generated file.

Moreover, you can translate the program _sum.imp_, which follows the K semantics
_imp-dico.lp_, into Dedukti as follow:

- _bash utilities/translate_pgm.sh imp-dico.lp sum.imp_

Note: Resulting outputs can be found in the folder _example/_.

Finally, you can run all the tests with _make test-lp_.\
If you interrupt a _kompile_ command, you may need to run _test-clean_.

# The files' hierarchy of KaMeLo

.\
├── main.ml: _The main algorithm to translate a semantics or an executable_\
├── Makefile\
├── README.md: _This file_\
├── example: _Examples of K semantics translation_\
├── src: _Source of KaMeLo. (See src/README.md)_\
├── utilities: _Some useful scripts_\
└── tests\
      ├── 001_imp-dico/: _A modified IMP semantics_\
      ├── 002_imp-lib/: _A IMP semantics that uses the K standard library_\
      └── gen_tests.sh: _Run this script to launch all the tests_

Note: The files "dune" and "dune-project" are just here to compile the OCaml
code.

# Paper

This translator is described in the following paper:\
A. Ledein, V. Blot, C. Dubois, _Vers une traduction de K en Dedukti_,\
JFLA 2022, Juin 2022, Périgord, France. (See http://jfla.inria.fr/jfla2022.html)

The following command is used to retrieve the version of the translator KaMeLo\
associated with the previous article:

git clone -b JFLA2022 https://gitlab.com/semantiko/kamelo.git
