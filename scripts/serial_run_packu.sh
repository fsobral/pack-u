#!/bin/sh

# This script runs the optimization problem for ALL the data files
# located in the given directory. It uses the configuration files
# located in the current directory. In the end, generates a directory
# with solutions and PDF files with the solutions found.
#
# It needs file 'packlib.asy' to draw the solution.

DATA_FILES=dados-reais/pedidos

OUTPUT_DIR='solutions'

# Clean previous runs

rm -fr ${OUTPUT_DIR}

rm -fr stats_all.txt

mkdir ${OUTPUT_DIR}


for dataf in \
    `find ${DATA_FILES} -type f -name '*.txt' -printf '%f\n'`; do

    cp -f ${DATA_FILES}/${dataf} data.txt

    NAME=`echo ${dataf} | sed -s 's/[.].\+$//g'`

    ./empacota.sh

    # ./pack > /dev/null

    cp -vf solution.pdf ${OUTPUT_DIR}/${NAME}.pdf

    cp -vf solution.csv ${OUTPUT_DIR}/${NAME}.csv

    STATS=`cat stats.txt`

    awk "{print ${NAME}\";\" \$0}" stats.txt >> stats_all.txt

done

mv -vf stats_all.txt ${OUTPUT_DIR}/.
