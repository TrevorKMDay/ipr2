#! /usr/bin/bash

for i in *.pdf ; do

    pdftoppm.exe -png "${i}" > "${i//pdf/png}"

done