#!/bin/sh

asy sol0*.asy

gs  -sOutputFile=solution.pdf -sDEVICE=pdfwrite                    \
    -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH sol0*.pdf

rm sol0*.*
