#!/bin/bash

set -x
rm -f save_*.F90 *.xml

for f in yomvdoz.F90 yomtoph.F90 yomsimphl.F90 yomrip.F90 yomphy.F90 yomphyds.F90 yomphy3.F90 yomphy2.F90 \
         yomphy1.F90 yomphy0.F90 yomparar.F90 yomnorgwd.F90 yommse.F90 yomlouis.F90 yomcvmnh.F90  \
         yomarphy.F90 yoerdi.F90

do
  ../scripts/loadsave.pl --dir . --types $f
done

for f in yomcst.F90    yomlouis.F90  yomphy0.F90   yomphy1.F90   yomphy2.F90   yomphy.F90    
do
  ../scripts/loadsave.pl --dir . --vars $f
done

rm save_*.F90 *.xml
