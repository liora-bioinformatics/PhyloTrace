#!/bin/bash
set -e

echo "Fetching current cgMLST schemes from https://www.cgmlst.org/ncs/ ..."

sudo  curl -s -L -A "Mozilla/5.0 (compatible; cgMLST-Monitor; +https://github.com/liora-bioinformatics)" \
  https://www.cgmlst.org/ncs/ |
  grep -o "<a href='https://www.cgmlst.org/ncs/schema/[^']*'" |
  sed "s/<a href='//; s/'$//" |
  sort -u > ./assets/fetch_cgmlst.txt

if [[ ! -s "./assets/fetch_cgmlst.txt" ]]; then
  echo "Error: No schemes fetched. Check curl / network / grep."
fi

echo "Comparing with local schemes"

# Extract fetched and local lists
FETCHED=$(grep -oP '.*/\K[a-zA-Z_]+(?=[0-9]*/?)'  ./assets/fetch_cgmlst.txt | sort)
LOCAL=$(sed 1d ./assets/cgmlst_schemes.csv | cut -d',' -f3 | sort)

# Compare
updated=$(comm -23 <(echo "$FETCHED" | sort) <(echo "$LOCAL" | tr -d '"' | sort))
removed=$(comm -13 <(echo "$FETCHED" | sort) <(echo "$LOCAL" | tr -d '"' | sort))

echo "$updated" > ./assets/updated_cgmlst.txt
echo "$removed" > ./assets/removed_cgmlst.txt

echo "Following schemes were added:"
[[ -z "$updated" ]] && echo "none" || echo "$updated"

echo "Following schemes were removed:"
[[ -z "$removed" ]] && echo "none" || echo "$removed"

exit 0