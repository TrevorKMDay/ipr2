#!/bin/bash

for i in *.pdf ; do

    magick convert ${i} \
        "${i//pdf/png}"

done
