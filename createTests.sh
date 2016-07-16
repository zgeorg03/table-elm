#!/bin/bash

files=$(readlink -f ./tests/*)
for f in $files

do
  filename="$(echo $f | cut -f1 -d'.' )"
  filetype="$(echo $f | cut -f2 -d'.' )"
  if [[ "$filetype" != "elm" ]] ; then continue; fi
  echo $f
  elm-make $f --output "$filename"."js"
done
