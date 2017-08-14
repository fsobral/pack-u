#!/bin/sh

# This script runs the optimization problem for the configuration
# files located in the same directory. In the end, generates a PDF
# file with the solution found.
#
# It needs file 'packlib.asy' to draw the solution.

rm sol0*.*

./pack

asy sol0*.asy

gs  -sOutputFile=solution.pdf -sDEVICE=pdfwrite                    \
    -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH sol0*.pdf
