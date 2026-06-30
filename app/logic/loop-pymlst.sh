#!/bin/bash

# Default values
IDENTITY=0.95
COVERAGE=0.9
env=pymlst_env

# --- Help Message ---
usage() {
    echo "Usage: $0 -d <database_path> [-i <identity>] [-c <coverage>] [-e <conda_env>] -- <genome_file> [<genome_file> ...]"
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
}

# --- Parse flags ---
while getopts "d:i:c:e:" opt; do
    case "$opt" in
        d) DB_PATH="$OPTARG" ;;
        i) IDENTITY="$OPTARG" ;;
        c) COVERAGE="$OPTARG" ;;
        e) env="$OPTARG" ;;
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
