#!/bin/bash
set -e

echo "Fetching PubMLST available schemes"
sudo pubmlstdownload update_schemes -force_refresh > ./assets/fetch_pubmlst.txt

echo "Comparing with local schemes"

# Extract fetched and local lists into variables to avoid process substitution issues
FETCHED=$(awk -F ' -> ' 'tolower($3) ~ /cgmlst/ {print $4}' ./assets/fetch_pubmlst.txt | sort)
LOCAL=$(cut -d ',' -f 4 ./assets/pubmlst_schemes.csv | grep -v "url" | sort)

# Compare
updated=$(comm -23 <(echo "$FETCHED") <(echo "$LOCAL"))
removed=$(comm -13 <(echo "$FETCHED") <(echo "$LOCAL"))

echo "$updated" > ./assets/updated_pubmlst.txt
echo "$removed" > ./assets/removed_pubmlst.txt

echo "Following schemes were added:"
[[ -z "$updated" ]] && echo "none" || echo "$updated"

echo "Following schemes were removed:"
[[ -z "$removed" ]] && echo "none" || echo "$removed"