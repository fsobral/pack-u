#!/bin/sh

# This script runs the optimization problem for ALL the data files
# located in the given directory. It uses the configuration files
# located in the current directory. In the end, generates a directory
# with solutions and PDF files with the solutions found.
#
# It needs file 'packlib.asy' to draw the solution.

DATA_FILES=../../dados-reais/pedidos

OUTPUT_DIR='solutions'

# Clean previous runs

rm -fr ${OUTPUT_DIR}

rm -fr stats_all.csv

mkdir ${OUTPUT_DIR}


for dataf in \
    `find ${DATA_FILES} -type f -name '*.txt' -printf '%f\n' | sort`; do

    cp -f ${DATA_FILES}/${dataf} data.txt

    NAME=`echo ${dataf} | sed -s 's/[.].\+$//g'`

    # Pure version with PDF generation
    #./run_packu.sh

    # Pure version without output
    #./packu > /dev/null

    # Cached version
    ../scripts/run_and_cache.py > /dev/null

    #cp -f solution.pdf ${OUTPUT_DIR}/${NAME}.pdf

    #cp -f solution.csv ${OUTPUT_DIR}/${NAME}.csv

    awk "{print ${NAME}\";\" \$0}" stats.csv >> stats_all.csv

done

mv -vf stats_all.csv ${OUTPUT_DIR}/.
