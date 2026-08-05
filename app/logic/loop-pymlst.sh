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
CLA_DB=""
CLA_SPEC=""

# AMR screening configurations (abritamr / AMRFinderPlus)
AMR_ENV=""
AMR_SPECIES=""
AMR_OUT=""

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Display command-line usage and option flags.
usage() {
    echo "Usage: $0 -d <database_path> [-i <identity>] [-c <coverage>] [-e <conda_env>] [-s <species>] [-m <cla_db_path>] [-M <cla_scheme_spec>] [-A <amr_env>] [-p <amr_species>] [-o <amr_out_dir>] -- <genome_file> [<genome_file> ...]"
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

while getopts "d:i:c:e:s:m:M:A:p:o:" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        i) IDENTITY="$OPTARG" ;;
        c) COVERAGE="$OPTARG" ;;
        e) env="$OPTARG" ;;
        s) SPECIES="$OPTARG" ;;
        m) CLA_DB="$OPTARG" ;;
        M) CLA_SPEC="$OPTARG" ;;
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
# The scheme is resolved beforehand in R (app/logic/mlst_repo.R) and handed over
# as a spec file, so this step only downloads the exact profile table and allele
# files it names and builds the reference locally with `claMLST create`.
# pyMLST's own `claMLST import` re-searches the repositories by fuzzy substring
# match and refuses to choose whenever more than one hit comes back, which loses
# the ST for every species holding more than one scheme (Enterococcus faecium)
# and can settle on a scheme from an entirely different genus.
if [[ -n "$CLA_SPEC" && -f "$CLA_SPEC" && -n "$CLA_DB" ]]; then
    CLA_REPO=""
    CLA_DATABASE=""
    CLA_SCHEME=""
    CLA_VERSION=""
    CLA_PROFILES=""
    CLA_LOCI=()
    while IFS= read -r line; do
        key=${line%%=*}
        value=${line#*=}
        case "$key" in
            repository) CLA_REPO="$value" ;;
            species)    [[ -n "$value" ]] && SPECIES="$value" ;;
            database)   CLA_DATABASE="$value" ;;
            scheme)     CLA_SCHEME="$value" ;;
            version)    CLA_VERSION="$value" ;;
            profiles)   CLA_PROFILES="$value" ;;
            locus)      CLA_LOCI+=("$value") ;;
        esac
    done < "$CLA_SPEC"

    echo "------------------------------------------------"
    echo "Building classical MLST scheme for: $SPECIES"
    echo "Scheme: $CLA_REPO / $CLA_DATABASE / $CLA_SCHEME (${#CLA_LOCI[@]} loci)"

    built=0
    if ! command -v curl >/dev/null 2>&1; then
        echo "Classical MLST: curl not found, cannot download the scheme."
    elif [[ -z "$CLA_PROFILES" || ${#CLA_LOCI[@]} -eq 0 ]]; then
        echo "Classical MLST: incomplete scheme specification."
    else
        CLA_TMP=$(mktemp -d)
        mkdir -p "$CLA_TMP/locus"
        downloaded=1
        if ! curl -fsSL --retry 3 --retry-delay 2 -o "$CLA_TMP/profiles.csv" "$CLA_PROFILES"; then
            echo "Classical MLST: download failed for the profile table."
            downloaded=0
        fi
        # claMLST create matches allele files to profile columns by file name, so
        # each locus keeps the name the repository gives it.
        if [[ "$downloaded" -eq 1 ]]; then
            for locus_url in "${CLA_LOCI[@]}"; do
                locus_name=${locus_url##*/}
                if ! curl -fsSL --retry 3 --retry-delay 2 \
                        -o "$CLA_TMP/locus/$locus_name.fasta" "$locus_url/alleles_fasta"; then
                    echo "Classical MLST: download failed for locus $locus_name."
                    downloaded=0
                    break
                fi
            done
        fi
        if [[ "$downloaded" -eq 1 ]] && "$CONDA" run -n "$env" claMLST create -f \
                -s "$SPECIES" -V "$CLA_VERSION" \
                "$CLA_DB" "$CLA_TMP/profiles.csv" "$CLA_TMP"/locus/*.fasta; then
            built=1
            # Kept for the ST lookup on finalize: claMLST create drops profile
            # rows whose locus is recorded as absent (allele 0), so the table
            # itself is the only place those STs can still be reached.
            cp "$CLA_TMP/profiles.csv" "$CLA_DB.profiles.tsv"
        fi
        rm -rf "$CLA_TMP"
    fi

    if [[ "$built" -eq 1 && -f "$CLA_DB" ]]; then
        echo "Classical MLST repository: $CLA_REPO"
        echo "Classical MLST scheme: $CLA_DATABASE ($CLA_SCHEME)"
        echo "Classical MLST scheme version: $CLA_VERSION"
    else
        CLA_DB=""
        echo "Classical MLST: reference build failed for '$SPECIES' (skipping ST calls)"
    fi
fi

# --- Report Allele Calling Tooling Provenance ---
# pyMLST drives both allele calling and the ST search; BLAT finds the loci and
# MAFFT aligns the incomplete ones. Reported unconditionally, so a run records
# what typed it even with classical MLST and AMR screening switched off. BLAT
# prints its banner and exits non-zero when given no arguments; MAFFT reports on
# stderr.
echo "------------------------------------------------"
echo "pyMLST version: $("$CONDA" run -n "$env" wgMLST --version 2>/dev/null | awk -F': ' '/Version/{print $2}')"
echo "BLAT version: $("$CONDA" run -n "$env" blat 2>&1 | sed -n 's/.*BLAT v\. *\([0-9.]*\).*/\1/p' | head -1)"
echo "MAFFT version: $("$CONDA" run -n "$env" mafft --version 2>&1 | head -1 | awk '{print $1}')"

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