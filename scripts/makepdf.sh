#!/bin/sh

asy sol9999.asy # Python Solution
asy   sol0*.asy

gs  -sOutputFile=solution.pdf -sDEVICE=pdfwrite                    \
    -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH sol9999.pdf sol0*.pdf

rm sol0*.*
