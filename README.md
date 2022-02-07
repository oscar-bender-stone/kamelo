KaMeLo  
========  

KaMeLo is a translator from **the semantical framework K** to **the logical framework Dedukti**.  
More precisely, KaMeLo takes as input **a Kore file**, that is the translation of a K semantic into **a Matching Logic theory**, and generates a Lambdapi file or a Dedukti file.  
More concretly, you can translate a K semantic, let say *imp.k*, into Dedukti as follow:  
  - *kompile imp.k*  
  - *cd kamelo*  
  - *make*  
  - *./KaMeLo ../imp-kompiled/definition.kore*  
