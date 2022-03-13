#! /bin/bash -e

tests_folder=tests
kamelo_script=KaMeLo
python_script=src/printing/rewrite.py # To rewrite beautifully a program
sem_root=sem_root # Racine par défaut utilisée pour tous les sous-dossiers
                  # afin de savoir où se trouve la racine des fichiers de
                  # sémantique. Nom à changer également dans les fonctions
                  # d'import de la traduction
extension=$1 #$(if [ $# = 1 ]; then echo $1 ; else echo "lp" ;fi)

  #######################################################################
  #    usage: ./gen_tests dk [one-test] ou ./gen_tests lp [one-test]    #
  #                                                                     #
  # Ce script a pour objectif de traduire les fichiers de tests en      # 
  # Dedukti.                                                            #
  #                                                                     #
  # Ces fichiers sont supposés être dans "tests_folder", lui-même       #
  # composé de dossiers dont le nom suit la nomenclature suivante :     #
  #   "nb_semName" ou "nb_M_semName"                                    #
  #      où "nb" est un nombre à 3 chiffres, et                         #
  #         "semName" est le nom de la sémantique.                      #
  # Si "sem_Name" est précédé de "M_", cela signifie que le fichier     #
  # Kore correspondant à la sémantique est obtenu à l'aide d'un         #
  # Makefile, ou plus précisément, à l'aide de la commande "make".      #
  # Des noms de sous-dossiers acceptables sont donc par exemple,        #
  # 001_max, 010_imp, ou encore 050_M_michelson.                        #
  # De plus, cas particulier : nous avons nécessairement                #
  # sous-dossier 000_KoreSyntax qui contient des fichiers à tester      #
  # pour s'assurer que KaMeLo est capable de parser un fichier Kore     #
  # correctement.                                                       #
  #                                                                     #
  # Chacun de ces sous-dossiers contient :                              #
  #   - Si le nom du sous-dossier est de la forme "nb_semName",         #
  #     un fichier "semName.k".                                         #
  #   - Si le nom du sous-dossier est de la forme "nb_M_semName",       #
  #     plusieurs fichiers constituant la sémantique et un Makfile.     #
  #   - un dossier "semName-exec/" contenant de nombreux programmes     #
  #     écrits dans le langage décrit par la sémantique.                #
  #     Les noms de ces programmes suivent la forme "pgrmName.semName". #
  #                                                                     #
  # La hiérarchie précédemment décrite est conservée lors de la         #
  # traduction en Dedukti. Toutes les extensions des fichiers           #
  # deviennent ".dk" ou ".lp" en fonction de l'option passée par la     #
  # ligne de commande.                                                  #
  # Ainsi, les duals du dossier "tests_folder" sont "dk-generated" et   #
  # et "lp-generated", et se trouvent dans "../tests_folder".           #
  #                                                                     #
  #######################################################################

nb_nomencla=5 # Nombre permettant de couper avec "cut" pour supprimer les chiffres de la nomenclature

# Création du dossier où seront les fichiers générés, 
# sans message d'erreur si le dossier existe déjà
gen_folder=$(if [ "$extension" = "dk" ]; then echo "dk-generated" ; else echo "lp-generated" ;fi)
mkdir -p $gen_folder

cd $tests_folder

for_test=$(if [ "$#" = 2 ];
    then echo $(find . -mindepth 1 -maxdepth 1 -type d -iname "$2" | sort -d | cut -c3-) ;
    else echo $(find . -mindepth 1 -maxdepth 1 -type d | sort -d | cut -c3-) ;
    fi)

# Itération sur chaque dossier présent dans "tests_folder"
for d in $for_test; do
  if [ $(echo $d | cut -c-4) = "000_" ]
  then continue
  else
   cd $d

   # Récupération du nom du dossier sans les chiffres
   semName=$(echo $d | cut -c$nb_nomencla-)

   # Création du dossier "semName-kompiled/", s'il n'existe pas déjà
   is_kompiled=false
   for subd in $(find . -mindepth 1 -maxdepth 1 -type d | cut -c3-); do
      if [ $subd = $semName-kompiled ]; then is_kompiled=true; fi
   done
   if [ $is_kompiled = false ]; then
      if [ $(echo $semName | cut -c-2) = "M_" ]
      then make ; semName=$(echo $semName | cut -c3-)
      else echo "\nCompilation of the semantic:" $semName.k ; kompile $semName.k
      fi
   fi

   # Récupération du fichier Kore qui contient la sémantique 
   #    = "semName-kompiled/definition.kore"
   cp $semName-kompiled/definition.kore $semName.kore 

   # Création du dual du sous-dossier courant
   cd ../..
   curr_gen_folder=$d-$extension  # 001_max-dk par exemple
   curr_exec_folder=$semName-exec # max-exec par exemple
   mkdir $curr_gen_folder
   mkdir $curr_gen_folder/$curr_exec_folder
   # Traduction de la sémantique
   echo "Translation of the semantic:" $semName.kore
   ./$kamelo_script -r $tests_folder/$d/$semName.kore
   # rm $tests_folder/$d/$semName.kore
   mv $semName.$extension $curr_gen_folder/
   #mv $tests_folder/$d/$semName.kore $curr_gen_folder/

   # Création du fichier de management de fichiers pour LP, si besoin
   LPpkg=lambdapi.pkg
   if [ $extension = "lp" ]; then echo -n "package_name = $sem_root\nroot_path    = $sem_root" > $LPpkg ;fi
   mv $LPpkg $curr_gen_folder/

   # Traduction des programmes se trouvant dans "curr_exec_folder"
   cd $tests_folder/$d/$curr_exec_folder
   for f in $(find . -mindepth 1 -type f | cut -c3-); do
      # Traduction vers Kore
      # pour utiliser krun, il faut être dans le dossier où se trouve "semName-kompiled/"
      cd ..
      echo "Translation of the program and its result:" $f
      new_name=${f%.*} # Suppression de l'extension (A faire avec la commande POSIX basename?)
      krun --depth 0 --output kore $curr_exec_folder/$f > ../../$curr_gen_folder/$curr_exec_folder/$new_name.kore
      krun           --output kore $curr_exec_folder/$f > ../../$curr_gen_folder/$curr_exec_folder/$new_name-res.kore
      # Fusion du programme et de son résultat, séparés par "\n@@@@@\n"
      cd ../..
      echo "\n@@@@@\n" > $curr_gen_folder/$curr_exec_folder/sep
      cat $curr_gen_folder/$curr_exec_folder/$new_name.kore $curr_gen_folder/$curr_exec_folder/sep $curr_gen_folder/$curr_exec_folder/$new_name-res.kore > $curr_gen_folder/$curr_exec_folder/tmp.kore
      mv $curr_gen_folder/$curr_exec_folder/tmp.kore $curr_gen_folder/$curr_exec_folder/$new_name.kore
      # Traduction vers Dedukti
      ./$kamelo_script -r --semantics $semName $curr_gen_folder/$curr_exec_folder/$new_name.kore
      python3 $python_script $curr_gen_folder/$curr_exec_folder/$new_name.$extension
      # Suppression des fichiers générés
      rm $curr_gen_folder/$curr_exec_folder/$new_name.kore
      rm $curr_gen_folder/$curr_exec_folder/$new_name-res.kore
      rm $curr_gen_folder/$curr_exec_folder/sep

      cd $tests_folder/$d/$curr_exec_folder
   done

   # Mettre le sous-dossier courant dans le dossier "gen_folder"
   cd ../../..
   rm -rf $gen_folder/$curr_gen_folder
   mv $curr_gen_folder -t $gen_folder

   cd $tests_folder
  fi
done

# Lambdapi check
cd ../$gen_folder
for d in $(find . -mindepth 1 -maxdepth 1 -type d | sort -d | cut -c3-); do
  cd $d
  semName=$(echo ${d%-lp} | cut -c$nb_nomencla-) # To delete "-lp"
  cd $semName-exec

  for pgm in $(find . -mindepth 1 -maxdepth 1 | sort -d) ; do
    lambdapi check $pgm
  done
done

cd ../..
