#!/bin/bash

# Default values
IDENTITY=0.95
COVERAGE=0.9
env=pymlst_env
SPECIES=""
REPO=pubmlst
CLA_DB=""

# --- Help Message ---
usage() {
    echo "Usage: $0 -d <database_path> [-i <identity>] [-c <coverage>] [-e <conda_env>] [-s <species>] [-r <pubmlst|pasteur>] [-m <cla_db_path>] -- <genome_file> [<genome_file> ...]"
    exit 1
}

# --- Function to run the wgMLST command ---
process_genome() {
    local db=$1
    local file=$2
    local id=$3
    local cov=$4

    # Dynamically handle extension (removes .fna, .fasta, or .fa)
    local strain_name=$(basename "$file" | sed 's/\.[^.]*$//')

    echo "------------------------------------------------"
    echo "Processing Strain: $strain_name"
    echo "Using Database: $db"

    # Use conda run to ensure the environment is used correctly
    conda run -n "$env" wgMLST add "$db" "$file" \
        --strain "$strain_name" \
        --identity "$id" \
        --coverage "$cov"

    # --- Classical MLST (best-effort) --------------------------------------
    # When a temporary claMLST reference DB was built for this run, also derive
    # the classical 7-gene Sequence Type for this strain. `claMLST search`
    # prints a small TSV to stdout (header row + one data row): ST is column 2
    # and the remaining columns are the per-gene allele calls. stderr (INFO
    # logging) is dropped so only the table is parsed. Never fails the strain -
    # a missing DB or no match just yields "NA".
    if [[ -n "$CLA_DB" && -f "$CLA_DB" ]]; then
        local cla_out st alleles
        cla_out=$(conda run -n "$env" claMLST search -i "$id" -c "$cov" "$CLA_DB" "$file" 2>/dev/null)
        st=$(echo "$cla_out" | awk -F'\t' 'NR==2{print $2}')
        alleles=$(echo "$cla_out" | awk -F'\t' \
            'NR==1{for (i=3;i<=NF;i++) g[i]=$i}
             NR==2{o="";for (i=3;i<=NF;i++){o=o (o==""?"":",") g[i]"="$i} print o}')
        [[ -z "$st" ]] && st="NA"
        echo "Classical MLST ST: $st"
        [[ -n "$alleles" ]] && echo "Classical MLST alleles: $alleles"
    fi
}

# --- Parse flags ---
while getopts "d:i:c:e:s:r:m:" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        i) IDENTITY="$OPTARG" ;;
        c) COVERAGE="$OPTARG" ;;
        e) env="$OPTARG" ;;
        s) SPECIES="$OPTARG" ;;
        r) REPO="$OPTARG" ;;
        m) CLA_DB="$OPTARG" ;;
        *) usage ;;
    esac
done
# Drop the parsed options (and the `--` guard); the genomes to type are the
# remaining positional arguments, resolved and de-duplicated by the caller.
shift $((OPTIND - 1))

# Validation
if [[ -z "$DB_PATH" ]] || [[ $# -eq 0 ]]; then
    echo "Error: Missing required arguments."
    usage
fi

# --- Build the classical MLST reference database ------------------------------
# Classical MLST rides along in this same run: build a claMLST reference DB for
# the scheme's species (via `claMLST import`) at the caller-provided path (-m),
# then search each genome against it in process_genome. The reference DB is a
# per-run artifact of allele calling - the R caller passes a temp path, reads the
# reference sequences / metadata it needs once this process exits, and deletes
# it. Best-effort throughout: any failure just disables ST calling and never
# aborts cgMLST typing. Skipped entirely when no species or no -m path is given.
if [[ -n "$SPECIES" && -n "$CLA_DB" ]]; then
    # Try the requested repository first, then fall back to the other one.
    if [[ "$REPO" == "pasteur" ]]; then
        REPOS=("pasteur" "pubmlst")
    else
        REPOS=("pubmlst" "pasteur")
    fi

    echo "------------------------------------------------"
    echo "Building classical MLST scheme for: $SPECIES"
    built=0
    for repo in "${REPOS[@]}"; do
        echo "Trying classical MLST repository: $repo"
        if conda run -n "$env" claMLST import --no-prompt -f -r "$repo" "$CLA_DB" "$SPECIES"; then
            built=1
            break
        fi
        echo "Classical MLST import from $repo failed."
    done

    if [[ "$built" -eq 1 && -f "$CLA_DB" ]]; then
        # Emit run-level provenance (parsed once by the R side). `claMLST info`
        # reports the repository actually used, the resolved species, and the
        # reference database's release date.
        cla_info=$(conda run -n "$env" claMLST info "$CLA_DB" 2>/dev/null)
        echo "Classical MLST repository: $(echo "$cla_info" | awk -F'\t' '$1=="source"{print $2}')"
        echo "Classical MLST scheme: $(echo "$cla_info" | awk -F'\t' '$1=="species"{print $2}')"
        echo "Classical MLST scheme version: $(echo "$cla_info" | awk -F'\t' '$1=="version"{print $2}')"
        echo "pyMLST version: $(conda run -n "$env" claMLST --version 2>/dev/null | awk -F': ' '/Version/{print $2}')"
    else
        # Disable per-genome search; R will find no file at this path.
        CLA_DB=""
        echo "Classical MLST: no scheme found for '$SPECIES' (skipping ST calls)"
    fi
fi

echo "Starting allele calling..."

# Process each genome file handed in. The caller passes only the assemblies
# that should actually be typed (e.g. excluding strains already in the base).
for genome_file in "$@"; do
    if [[ -f "$genome_file" ]]; then
        # CORRECTED ORDER: DB first, then File
        process_genome "$DB_PATH" "$genome_file" "$IDENTITY" "$COVERAGE"
    else
        echo "Error: File $genome_file not found."
    fi
done

echo "------------------------------------------------"
echo "Done!"
