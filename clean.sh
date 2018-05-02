#!/bin/csh
#
rm -f `find . -name \*.a`;
rm -f `find . -name \*.so`;
rm -f `find . -name \*.mod`;
rm -f `find . -name \*~`;
rm -f astro/src/astro/astroapi-perl_0.10-1_amd64.*;
cd astro/src && make clean;
#
