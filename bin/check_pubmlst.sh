#!/bin/bash

### Retrieve vailable schemes from PubMLST
echo "Fetching PubMLST available schemes"
pubmlstdownload update_schemes -force_refresh > ./assets/fetch_pubmlst.txt

### Compare with local/tracked schemes
echo "Comparing with local schemes"

# Updated schemes
updated=$(comm -23 <(awk -F ' -> ' 'tolower($3) ~ /cgmlst/ {print $4}' ./assets/fetch_pubmlst.txt | sort) \
                       <(cut -d ',' -f 4 ./assets/pubmlst_schemes.csv | grep -v "url" | sort))

echo "$updated" > ./assets/updated_pubmlst.txt

# Removed schemes 
removed=$(comm -13 <(awk -F ' -> ' 'tolower($3) ~ /cgmlst/ {print $4}' ./assets/fetch_pubmlst.txt | sort) \
                       <(cut -d ',' -f 4 ./assets/pubmlst_schemes.csv | grep -v "url" | sort))

echo "$removed" > ./assets/removed_pubmlst.txt

echo "Following schemes were added:"
if [ -z "$updated" ]; then
    echo "none"
else
    echo "$updated"
fi

echo "Following schemes were removed:"
if [ -z "$removed" ]; then
    echo "none"
else
    echo "$removed"
fi

exit 0