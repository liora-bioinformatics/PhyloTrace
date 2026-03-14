#!/bin/bash

# Default values
IDENTITY=0.95
COVERAGE=0.9

# --- Help Message ---
usage() {
    echo "Usage: $0 -d <database_path> [-g <genome_directory> | -f <single_file>] [-i <identity>] [-c <coverage>]"
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
    conda run -n pymlst wgMLST add "$db" "$file" \
        --strain "$strain_name" \
        --identity "$id" \
        --coverage "$cov"
}

# --- Parse flags ---
while getopts "d:g:f:i:c:" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        g) GENOME_DIR="$OPTARG" ;;
        f) SINGLE_FILE="$OPTARG" ;;
        i) IDENTITY="$OPTARG" ;;
        c) COVERAGE="$OPTARG" ;;
        *) usage ;;
    esac
done

# Validation
if [[ -z "$DB_PATH" ]] || [[ -z "$GENOME_DIR" && -z "$SINGLE_FILE" ]]; then
    echo "Error: Missing required arguments."
    usage
fi

echo "Starting allele calling..."

# Logic 1: Process single file
if [[ -n "$SINGLE_FILE" && -f "$SINGLE_FILE" ]]; then
    # CORRECTED ORDER: DB first, then File
    process_genome "$DB_PATH" "$SINGLE_FILE" "$IDENTITY" "$COVERAGE"
elif [[ -n "$SINGLE_FILE" ]]; then
    echo "Error: File $SINGLE_FILE not found."
fi

# Logic 2: Process directory
if [[ -n "$GENOME_DIR" && -d "$GENOME_DIR" ]]; then
    # Look for common genome extensions
    for genome_file in "$GENOME_DIR"/*.{fasta,fna,fa}; do
        [ -e "$genome_file" ] || continue
        # CORRECTED ORDER: DB first, then File
        process_genome "$DB_PATH" "$genome_file" "$IDENTITY" "$COVERAGE"
    done
fi

echo "------------------------------------------------"
echo "Done!"