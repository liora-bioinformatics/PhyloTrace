#!/bin/bash
KNOWN_FILE="assets/pubmlst_schemes.csv"
TEMP_LIVE="live_schemes.tmp"
touch "$KNOWN_FILE"

# 1. Fetch live data
DB_DATA=$(curl -s https://rest.pubmlst.org/db | jq -r '.[].databases[] | select(.name | test("_seqdef$")) | "\(.href)\t\(.description)"')

while IFS=$'\t' read -r DB_URL DB_DESC; do
    CLEAN_NAME=$(echo "$DB_DESC" | sed 's/ sequence\/profile definitions//g')
    SCHEMES_JSON=$(curl -s "${DB_URL}/schemes")
    SCHEMES=$(echo "$SCHEMES_JSON" | jq -c '.schemes[]? | select(.description | test("cgmlst"; "i"))')
    
    if [[ -n "$SCHEMES" ]]; then
        while read -r row; do
            S_URL=$(echo "$row" | jq -r '.scheme')
            S_DESC=$(echo "$row" | jq -r '.description')
            L_COUNT=$(curl -s "$S_URL" | jq '.loci | length')
            echo "Found: $CLEAN_NAME | $S_DESC | $L_COUNT loci"
            echo -e "${S_URL}\t${L_COUNT}\t${S_DESC}\t${CLEAN_NAME}" >> "$TEMP_LIVE"
        done <<< "$SCHEMES"
    fi
done <<< "$DB_DATA"

echo -e "\n--- DISCREPANCY REPORT ---"
# Detect columns in CSV (handle R row names)
HEADER=$(head -n 1 "$KNOWN_FILE")
COMMA_COUNT=$(echo "$HEADER" | tr -cd ',' | wc -c)

# Check for NEW
while IFS=$'\t' read -r l_url l_count l_desc l_db; do
    if ! grep -q "$l_url" "$KNOWN_FILE"; then
        echo "  + NEW: $l_db | $l_desc | URL: $l_url"
    fi
done < "$TEMP_LIVE"

# Check for REMOVED
tail -n +2 "$KNOWN_FILE" | while IFS=',' read -r col1 col2 col3 col4; do
    if [ "$COMMA_COUNT" -eq 3 ]; then current_url="$col3"; current_species="$col2";
    else current_url="$col2"; current_species="$col1"; fi
    
    current_url=$(echo "$current_url" | tr -d '"')
    if ! grep -q "^$current_url" "$TEMP_LIVE"; then
        echo "  - REMOVED: $current_species ($current_url)"
    fi
done
rm "$TEMP_LIVE"