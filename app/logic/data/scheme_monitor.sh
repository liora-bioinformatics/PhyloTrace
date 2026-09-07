#! /bin/bash

# Count number of schemes in csv
COUNT=$(tail -n +2 app/logic/data/cgmlst_schemes.csv | wc -l | xargs)

# Count number of schemes from online repo
MONITORED=$(curl -s -L -A "Mozilla/5.0 (compatible; cgMLST-Monitor; +https://github.com/liora-bioinformatics)"   https://www.cgmlst.org/ncs/ |   grep -o "<a href='https://www.cgmlst.org/ncs/schema/[^']*'" | wc -l)

# Calculate diff
DIFF=$(($COUNT - $MONITORED))
# if ! [ $DIFF -eq 0 ]; then
#     echo $DIFF
# fi
echo $DIFF
