#!/bin/bash

# ==============================================================================
# Executable and Environment Resolution
# ==============================================================================

# Resolve the base conda executable. CONDA_EXE (set during conda init or activation)
# points reliably to the base installation, avoiding env-local pathing issues.
CONDA="${CONDA_EXE:-conda}"

env=PhyloTrace
SCHEME=""
SCHEME_URL=""
FORCE=""

# Stall limit for the fallback download, matching the 600 s read timeout pyMLST
# gives `wgMLST import`. It aborts only a transfer that receives no data for that
# long, never a slow one: the largest archive (Escherichia coli, ~385 MB) takes
# about a minute.
STALL_SECONDS=600

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Display command-line usage and option flags.
usage() {
    echo "Usage: $0 -d <database_path> -n <scheme_name> [-u <scheme_url>] [-e <conda_env>] [-f]"
    exit 1
}

# ==============================================================================
# Option Parsing and Validation
# ==============================================================================

while getopts "d:n:u:e:f" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        n) SCHEME="$OPTARG" ;;
        u) SCHEME_URL="$OPTARG" ;;
        e) env="$OPTARG" ;;
        f) FORCE="--force" ;;
        *) usage ;;
    esac
done

if [[ -z "$DB_PATH" || -z "$SCHEME" ]]; then
    echo "Error: Missing required arguments."
    usage
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# ==============================================================================
# Import from cgmlst.org
# ==============================================================================

echo "Scheme download: importing $SCHEME"
"$CONDA" run --no-capture-output -n "$env" wgMLST import $FORCE --no-prompt \
    "$DB_PATH" "$SCHEME" 2>&1 | tee "$TMP/import.log"
import_status=${PIPESTATUS[0]}

if [[ "$import_status" -eq 0 ]]; then
    echo "Scheme download: done (wgMLST import)"
    exit 0
fi

# ==============================================================================
# Fallback: Build from the Exact Scheme URL
# ==============================================================================

# Two distinct pyMLST limitations land here. (1) `wgMLST import` matches the
# scheme by substring and so cannot pick a scheme whose name is a prefix of
# another's ("Citrobacter freundii" vs "Citrobacter freundii/portucalensis/
# braakii/europaeus"). (2) pyMLST downloads the allele archive itself with a
# hardcoded 600 s *wall-clock* timeout (requests.get(..., timeout=600) in
# pymlst/common/web.py) rather than a stall detector, so a large scheme on a
# merely slow connection fails even though data was still arriving; pyMLST
# exposes no flag to raise it. Our own curl fallback below uses a stall-based
# timeout instead, so it succeeds where pyMLST's `import` gives up. The exact
# scheme URL is resolved in R (scheme_url() in app/logic/scheme_browser.R);
# this downloads it and builds the reference with `wgMLST create`. Any other
# import failure is final.
if ! grep -qE "More than 1 result found|took too long to respond" "$TMP/import.log" \
        || [[ -z "$SCHEME_URL" ]]; then
    exit "$import_status"
fi

echo "Scheme download: import failed, building from $SCHEME_URL instead"

if ! command -v curl >/dev/null 2>&1; then
    echo "Error: curl not found, cannot download the scheme."
    exit 1
fi
if ! command -v unzip >/dev/null 2>&1; then
    echo "Error: unzip not found, cannot extract the scheme."
    exit 1
fi

if ! curl -fsSL --connect-timeout 60 \
        --speed-limit 1 --speed-time "$STALL_SECONDS" \
        --retry 2 --retry-delay 5 \
        -o "$TMP/alleles.zip" "${SCHEME_URL}alleles"; then
    echo "Error: download failed for the scheme alleles (no data for ${STALL_SECONDS} s, or the server refused)."
    exit 1
fi

mkdir "$TMP/fas"
if ! unzip -q -o "$TMP/alleles.zip" -d "$TMP/fas"; then
    echo "Error: could not extract the scheme archive."
    exit 1
fi

# Each locus file can hold several allele sequences; pyMLST's own importer
# keeps only the first record per file as the coregene reference template, so
# this mirrors it rather than picking a different allele.
COREGENE="$TMP/coregene.fasta"
: > "$COREGENE"
for fasta in "$TMP/fas"/*; do
    [[ -f "$fasta" ]] || continue
    name=$(basename "$fasta" .fasta)
    awk -v name="$name" '
        /^>/ {
            n++
            if (n == 1) { print "> " name; next }
            print ""
            exit
        }
        n == 1 { printf "%s", $0 }
        END { if (n == 1) print "" }
    ' "$fasta" >> "$COREGENE"
done

if [[ ! -s "$COREGENE" ]]; then
    echo "Error: no usable locus sequences found in the scheme archive."
    exit 1
fi

if "$CONDA" run --no-capture-output -n "$env" wgMLST create $FORCE \
        -s "$SCHEME" "$DB_PATH" "$COREGENE"; then
    echo "Scheme download: done (wgMLST create)"
else
    exit 1
fi
