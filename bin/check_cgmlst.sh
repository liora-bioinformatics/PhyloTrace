#!/usr/bin/env bash

set -euo pipefail

KNOWN_FILE="$SCRIPT_DIR/cgmlst_schemes.txt"
TEMP_FILE="$SCRIPT_DIR/.cgmlst_new.txt"
NEW_FILE="$SCRIPT_DIR/.new_schemes.txt"
REMOVED_FILE="$SCRIPT_DIR/.removed_schemes.txt"

echo "Fetching current cgMLST schemes from https://www.cgmlst.org/ncs/ ..."

curl -s -L -A "Mozilla/5.0 (compatible; cgMLST-Monitor; +https://github.com/liora-bioinformatics)" \
  https://www.cgmlst.org/ncs/ |
  grep -o "<a href='https://www.cgmlst.org/ncs/schema/[^']*'" |
  sed "s/<a href='//; s/'$//" |
  sort -u > "$TEMP_FILE"

if [[ ! -s "$TEMP_FILE" ]]; then
  echo "Error: No schemes fetched. Check curl / network / grep."
  exit 1
fi

NEW_COUNT=$(wc -l < "$TEMP_FILE")
KNOWN_COUNT=$(wc -l < "$KNOWN_FILE" 2>/dev/null || echo 0)

echo "Found $NEW_COUNT current schemes (previously known: $KNOWN_COUNT)"

if [[ ! -s "$KNOWN_FILE" ]]; then
  echo "First run — saving current list as baseline."
  mv "$TEMP_FILE" "$KNOWN_FILE"
  exit 0
fi

# Compare
if cmp -s "$KNOWN_FILE" "$TEMP_FILE"; then
  echo "No changes detected."
  rm -f "$TEMP_FILE"
  exit 0
fi

echo "Changes detected."

comm -23 "$TEMP_FILE" "$KNOWN_FILE" > "$NEW_FILE"
comm -13 "$TEMP_FILE" "$KNOWN_FILE" > "$REMOVED_FILE"

if [[ -s "$NEW_FILE" ]]; then
  echo "Updated schemes:"
  cat "$NEW_FILE"
fi

if [[ -s "$REMOVED_FILE" ]]; then
  echo "Removed schemes:"
  cat "$REMOVED_FILE"
fi

echo ""
echo "Update tracked schemes with"
echo "mv '$TEMP_FILE' '$KNOWN_FILE'"

rm -f "$TEMP_FILE" "$NEW_FILE" "$REMOVED_FILE"

exit 0