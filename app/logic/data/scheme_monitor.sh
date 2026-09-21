#!/bin/bash

CSV_PATH="app/logic/data/cgmlst_schemes.csv"

# Fetch online schema names
ONLINE_SCHEMES=$(curl -s -L -A "Mozilla/5.0 (compatible; cgMLST-Monitor; +https://github.com/liora-bioinformatics)" https://www.cgmlst.org/ncs/ \
  | grep -o "https://www.cgmlst.org/ncs/schema/[^/'\"]*" \
  | awk -F'/' '{print $NF}' \
  | grep -v '^$' \
  | sort -u)

# Extract local schema names from the 'abb' column (column 3)
LOCAL_SCHEMES=$(tail -n +2 "$CSV_PATH" | awk -F',' '{print $3}' | tr -d '"\r' | sort -u)

# Find missing online schemes locally
MISSING_LOCALLY=$(comm -13 <(echo "$LOCAL_SCHEMES") <(echo "$ONLINE_SCHEMES") | tr '\n' ' ' | xargs)

# Find local schemes missing online
MISSING_ONLINE=$(comm -23 <(echo "$LOCAL_SCHEMES") <(echo "$ONLINE_SCHEMES") | tr '\n' ' ' | xargs)

# Calculate net count difference (Local - Online)
COUNT_LOCAL=$(echo "$LOCAL_SCHEMES" | grep -c .)
COUNT_ONLINE=$(echo "$ONLINE_SCHEMES" | grep -c .)
DIFF=$((COUNT_LOCAL - COUNT_ONLINE))

# Output key-value pairs formatted for bash or GitHub outputs
echo "DIFF=$DIFF"
echo "MISSING_LOCALLY=${MISSING_LOCALLY:-None}"
echo "MISSING_ONLINE=${MISSING_ONLINE:-None}"