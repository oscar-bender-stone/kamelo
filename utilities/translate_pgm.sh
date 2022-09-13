#! /bin/bash -e

# Requirements:
#   - Run this script at the root.
#   - The first  argument is the Kore file of the K semantics.
#   - The second argument is the program which needs to be translated.

kamelo_script=KaMeLo
python_script=src/printing/rewrite.py # To rewrite beautifully a program
sem_root=sem_root # Racine par défaut utilisée pour tous les sous-dossiers
                  # afin de savoir où se trouve la racine des fichiers de
                  # sémantique. Nom à changer également dans les fonctions
                  # d'import de la traduction

extension=lp

semantics=$1
pgm=$2

# Some colors
cyanfonce='\e[0;36m'
neutre='\e[0;m'

  # Traduction du programme et de son résultat
  echo -e "${cyanfonce}Translation of the program and its result:${neutre}" $pgm
  new_name=${pgm%.*} # Suppression de l'extension (A faire avec la commande POSIX basename?)
  krun --depth 0 --output kore $pgm >  $new_name.kore
  krun           --output kore $pgm >> $new_name.kore

  # Traduction vers Dedukti
  if [ ! -f $kamelo_script ];
  then
    echo -e "${cyanfonce}Generate the executable${neutre}"
    make
  fi

  ./$kamelo_script -r --semantics ${semantics%.*} $new_name.kore
  python3 $python_script $new_name.$extension

  # Suppression des fichiers générés
  rm $new_name.kore

  # Création du fichier de management de fichiers pour LP, si besoin
  echo -e "${cyanfonce}Generate the file lambdapi.pkg${neutre}"
  LPpkg=lambdapi.pkg
  if [ $extension = "lp" ]; then
    echo "package_name = $sem_root" >  $LPpkg ;
    echo "root_path    = $sem_root" >> $LPpkg ;fi

  # Lambdapi check
  echo -e "${cyanfonce}Beginning of Lambdapi check..."
  lambdapi check --no-warnings -v 0 $new_name.$extension
  echo -e "${cyanfonce}...ending of Lambdapi check."
