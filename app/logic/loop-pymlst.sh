#!/bin/bash

# ==============================================================================
# Executable and Environment Resolution
# ==============================================================================

# Resolve the base conda executable. CONDA_EXE (set during conda init or activation)
# points reliably to the base installation, avoiding env-local pathing issues.
CONDA="${CONDA_EXE:-conda}"

# Default parameters
IDENTITY=0.95
COVERAGE=0.9
env=PhyloTrace
SPECIES=""
REPO=pubmlst
CLA_DB=""

# AMR screening configurations (abritamr / AMRFinderPlus)
AMR_ENV=""
AMR_SPECIES=""
AMR_OUT=""

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Display command-line usage and option flags.
usage() {
    echo "Usage: $0 -d <database_path> [-i <identity>] [-c <coverage>] [-e <conda_env>] [-s <species>] [-r <pubmlst|pasteur>] [-m <cla_db_path>] [-A <amr_env>] [-p <amr_species>] [-o <amr_out_dir>] -- <genome_file> [<genome_file> ...]"
    exit 1
}

#' Execute pipeline steps (cgMLST, classical MLST, and AMR) for a single genome.
#'
#' @param db Path to target SQLite database[cite: 12].
#' @param file Path to input FASTA assembly file[cite: 12].
#' @param id Identity threshold for allele calling[cite: 12].
#' @param cov Coverage threshold for allele calling[cite: 12].
process_genome() {
    local db=$1
    local file=$2
    local id=$3
    local cov=$4

    # Extract isolate name from filename (strip extensions: .fna, .fasta, .fa)
    local strain_name=$(basename "$file" | sed 's/\.[^.]*$//')

    # Record start timestamp for per-strain execution duration tracking
    local strain_start=$(date +%s)

    echo "------------------------------------------------"
    echo "Processing Strain: $strain_name"
    echo "Using Database: $db"

    # --- Step 1: cgMLST Allele Calling ---
    "$CONDA" run -n "$env" wgMLST add "$db" "$file" \
        --strain "$strain_name" \
        --identity "$id" \
        --coverage "$cov"

    # --- Step 2: Classical MLST (Best-Effort) ---
    if [[ -n "$CLA_DB" && -f "$CLA_DB" ]]; then
        local cla_out st alleles
        cla_out=$("$CONDA" run -n "$env" claMLST search -i "$id" -c "$cov" "$CLA_DB" "$file" 2>/dev/null)
        st=$(echo "$cla_out" | awk -F'\t' 'NR==2{print $2}')
        alleles=$(echo "$cla_out" | awk -F'\t' \
            'NR==1{for (i=3;i<=NF;i++) g[i]=$i}
             NR==2{o="";for (i=3;i<=NF;i++){o=o (o==""?"":",") g[i]"="$i} print o}')
        [[ -z "$st" ]] && st="NA"
        echo "Classical MLST ST: $st"
        [[ -n "$alleles" ]] && echo "Classical MLST alleles: $alleles"
    fi

    # --- Step 3: AMR Screening (Best-Effort via abritamr / AMRFinderPlus) ---
    if [[ -n "$AMR_OUT" && -n "$AMR_ENV" ]]; then
        local amr_prefix="$AMR_OUT/$strain_name"
        local amr_cores amr_jobs
        amr_cores=$(nproc 2>/dev/null || echo 2)
        amr_jobs=$(( amr_cores - 2 ))
        (( amr_jobs < 1 )) && amr_jobs=1
        
        echo "AMR: screening $strain_name"
        if timeout -k 30 1200 "$CONDA" run --no-capture-output -n "$AMR_ENV" \
                abritamr run -c "$file" -px "$amr_prefix" \
                ${AMR_SPECIES:+--species "$AMR_SPECIES"} --jobs "$amr_jobs" \
                >/dev/null 2>&1 </dev/null; then
            local n_amr=0
            if [[ -f "$amr_prefix/amrfinder.out" ]]; then
                n_amr=$(($(wc -l < "$amr_prefix/amrfinder.out") - 1))
                (( n_amr < 0 )) && n_amr=0
            fi
            echo "AMR: done $strain_name ($n_amr elements)"
        else
            echo "AMR: failed $strain_name"
        fi
    fi

    # Report completion timing sentinels
    echo "Strain elapsed: $(( $(date +%s) - strain_start ))"
    echo "Strain finished: $(date +%H:%M:%S)"
}

# ==============================================================================
# Option Parsing and Validation
# ==============================================================================

while getopts "d:i:c:e:s:r:m:A:p:o:" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        i) IDENTITY="$OPTARG" ;;
        c) COVERAGE="$OPTARG" ;;
        e) env="$OPTARG" ;;
        s) SPECIES="$OPTARG" ;;
        r) REPO="$OPTARG" ;;
        m) CLA_DB="$OPTARG" ;;
        A) AMR_ENV="$OPTARG" ;;
        p) AMR_SPECIES="$OPTARG" ;;
        o) AMR_OUT="$OPTARG" ;;
        *) usage ;;
    esac
done

shift $((OPTIND - 1))

# Require database path and at least one genome input file
if [[ -z "$DB_PATH" ]] || [[ $# -eq 0 ]]; then
    echo "Error: Missing required arguments."
    usage
fi

# ==============================================================================
# Pipeline Execution Setup
# ==============================================================================

# --- Build Classical MLST Reference Database ---
if [[ -n "$SPECIES" && -n "$CLA_DB" ]]; then
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
        if "$CONDA" run -n "$env" claMLST import --no-prompt -f -r "$repo" "$CLA_DB" "$SPECIES"; then
            built=1
            break
        fi
        echo "Classical MLST import from $repo failed."
    done

    if [[ "$built" -eq 1 && -f "$CLA_DB" ]]; then
        cla_info=$("$CONDA" run -n "$env" claMLST info "$CLA_DB" 2>/dev/null)
        echo "Classical MLST repository: $(echo "$cla_info" | awk -F'\t' '$1=="source"{print $2}')"
        echo "Classical MLST scheme: $(echo "$cla_info" | awk -F'\t' '$1=="species"{print $2}')"
        echo "Classical MLST scheme version: $(echo "$cla_info" | awk -F'\t' '$1=="version"{print $2}')"
        echo "pyMLST version: $("$CONDA" run -n "$env" claMLST --version 2>/dev/null | awk -F': ' '/Version/{print $2}')"
    else
        CLA_DB=""
        echo "Classical MLST: no scheme found for '$SPECIES' (skipping ST calls)"
    fi
fi

# --- Report AMR Tooling Provenance ---
if [[ -n "$AMR_OUT" && -n "$AMR_ENV" ]]; then
    echo "------------------------------------------------"
    echo "AMR screening enabled (env: $AMR_ENV, species: ${AMR_SPECIES:-none/fallback})"
    echo "AMR abritamr version: $("$CONDA" run -n "$AMR_ENV" abritamr --version 2>/dev/null | awk '{print $NF}')"
    echo "AMR finder version: $("$CONDA" run -n "$AMR_ENV" amrfinder --version 2>/dev/null | tr -d '\r')"
    echo "AMR database version: $("$CONDA" run -n "$AMR_ENV" python -c "import abritamr, os; d = os.path.join(os.path.dirname(abritamr.__file__), 'db', 'amrfinderplus', 'data'); print(sorted(os.listdir(d))[-1])" 2>/dev/null)"
fi

# ==============================================================================
# Main Processing Loop
# ==============================================================================

echo "Starting allele calling..."

for genome_file in "$@"; do
    if [[ -f "$genome_file" ]]; then
        process_genome "$DB_PATH" "$genome_file" "$IDENTITY" "$COVERAGE"
    else
        echo "Error: File $genome_file not found."
    fi
done

echo "------------------------------------------------"
echo "Done!"