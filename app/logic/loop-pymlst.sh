#!/bin/bash

# Resolve the base conda executable. A full conda can live inside an activated
# env (e.g. <root>/envs/PhyloTrace/bin/conda) and be first on PATH; that
# env-local conda treats the env as its root prefix, so `conda run -n <env>`
# looks for <root>/envs/PhyloTrace/envs/<env> and fails with
# EnvironmentLocationNotFound. CONDA_EXE (set by `conda init`/activation) always
# points at the base conda, so prefer it and fall back to a bare `conda`.
CONDA="${CONDA_EXE:-conda}"

# Default values
IDENTITY=0.95
COVERAGE=0.9
env=PhyloTrace
SPECIES=""
REPO=pubmlst
CLA_DB=""
# AMR screening (abritamr / AMRFinderPlus) rides along when -o is given.
AMR_ENV=""
AMR_SPECIES=""
AMR_OUT=""

# --- Help Message ---
usage() {
    echo "Usage: $0 -d <database_path> [-i <identity>] [-c <coverage>] [-e <conda_env>] [-s <species>] [-r <pubmlst|pasteur>] [-m <cla_db_path>] [-A <amr_env>] [-p <amr_species>] [-o <amr_out_dir>] -- <genome_file> [<genome_file> ...]"
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

    # Wall-clock start of this strain's whole pipeline (cgMLST + classical MLST +
    # AMR). The per-step [INFO] timestamps only cover allele calling, so the R
    # side reads the "Strain elapsed"/"Strain finished" sentinels emitted at the
    # end of this function to report the full per-strain duration.
    local strain_start=$(date +%s)

    echo "------------------------------------------------"
    echo "Processing Strain: $strain_name"
    echo "Using Database: $db"

    # Use conda run to ensure the environment is used correctly
    "$CONDA" run -n "$env" wgMLST add "$db" "$file" \
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
        cla_out=$("$CONDA" run -n "$env" claMLST search -i "$id" -c "$cov" "$CLA_DB" "$file" 2>/dev/null)
        st=$(echo "$cla_out" | awk -F'\t' 'NR==2{print $2}')
        alleles=$(echo "$cla_out" | awk -F'\t' \
            'NR==1{for (i=3;i<=NF;i++) g[i]=$i}
             NR==2{o="";for (i=3;i<=NF;i++){o=o (o==""?"":",") g[i]"="$i} print o}')
        [[ -z "$st" ]] && st="NA"
        echo "Classical MLST ST: $st"
        [[ -n "$alleles" ]] && echo "Classical MLST alleles: $alleles"
    fi

    # --- AMR screening (best-effort) ---------------------------------------
    # When an AMR output dir (-o) and env (-A) were given, screen this genome
    # with abritamr. abritamr always runs `amrfinder --plus` (acquired AMR +
    # virulence + stress/metal); `--species` (when supported for this organism)
    # additionally calls point mutations. Outputs land in "$AMR_OUT/$strain":
    # amrfinder.out + summary_{matches,partials,virulence}.txt, read back by the
    # R caller once this process exits. Never fails the strain.
    #
    # Threads matter a lot: abritamr passes `--jobs` straight to amrfinder's
    # `--threads`, and a single-threaded `--plus --organism` screen of a several-
    # megabase genome takes minutes (HMMER over thousands of profiles + BLAST +
    # point mutations). Screening is sequential per genome, and cgMLST/claMLST
    # for this strain are already done by now, so the cores are free - use all
    # of them but one, leaving a core for the OS and the Shiny UI to stay
    # responsive. `timeout` bounds a genuinely stuck screen so AMR can never hang
    # the whole typing run: on expiry the strain is simply reported "failed" and
    # the loop moves on. `--no-capture-output` streams straight to the /dev/null
    # sink (no conda buffering); `</dev/null` gives the children a closed stdin.
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

    # Whole-pipeline timing for this strain (allele calling + classical MLST +
    # AMR), read by parse_typing_log() for the "Elapsed" / "Finished" columns.
    echo "Strain elapsed: $(( $(date +%s) - strain_start ))"
    echo "Strain finished: $(date +%H:%M:%S)"
}

# --- Parse flags ---
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
        if "$CONDA" run -n "$env" claMLST import --no-prompt -f -r "$repo" "$CLA_DB" "$SPECIES"; then
            built=1
            break
        fi
        echo "Classical MLST import from $repo failed."
    done

    if [[ "$built" -eq 1 && -f "$CLA_DB" ]]; then
        # Emit run-level provenance (parsed once by the R side). `claMLST info`
        # reports the repository actually used, the resolved species, and the
        # reference database's release date.
        cla_info=$("$CONDA" run -n "$env" claMLST info "$CLA_DB" 2>/dev/null)
        echo "Classical MLST repository: $(echo "$cla_info" | awk -F'\t' '$1=="source"{print $2}')"
        echo "Classical MLST scheme: $(echo "$cla_info" | awk -F'\t' '$1=="species"{print $2}')"
        echo "Classical MLST scheme version: $(echo "$cla_info" | awk -F'\t' '$1=="version"{print $2}')"
        echo "pyMLST version: $("$CONDA" run -n "$env" claMLST --version 2>/dev/null | awk -F': ' '/Version/{print $2}')"
    else
        # Disable per-genome search; R will find no file at this path.
        CLA_DB=""
        echo "Classical MLST: no scheme found for '$SPECIES' (skipping ST calls)"
    fi
fi

# --- AMR provenance -----------------------------------------------------------
# Emit the abritamr / AMRFinderPlus / reference-DB versions once (parsed once by
# the R side and stamped onto every stored AMR row). The DB version is the newest
# dated directory in abritamr's bundled AMRFinder data dir.
if [[ -n "$AMR_OUT" && -n "$AMR_ENV" ]]; then
    echo "------------------------------------------------"
    echo "AMR screening enabled (env: $AMR_ENV, species: ${AMR_SPECIES:-none/fallback})"
    echo "AMR abritamr version: $("$CONDA" run -n "$AMR_ENV" abritamr --version 2>/dev/null | awk '{print $NF}')"
    echo "AMR finder version: $("$CONDA" run -n "$AMR_ENV" amrfinder --version 2>/dev/null | tr -d '\r')"
    echo "AMR database version: $("$CONDA" run -n "$AMR_ENV" python -c "import abritamr, os; d = os.path.join(os.path.dirname(abritamr.__file__), 'db', 'amrfinderplus', 'data'); print(sorted(os.listdir(d))[-1])" 2>/dev/null)"
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
