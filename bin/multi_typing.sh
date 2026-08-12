#!/bin/bash

APP_LOCAL_SHARE_DIR="$HOME/.local/share/phylotrace"

# Get variables
base_path=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 
  'multi_typing_df.rds'))[['wd']])")

scheme=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 
  'multi_typing_df.rds'))[['scheme']])")

genome_folder=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 
  'multi_typing_df.rds'))[['genome_folder']])")

genome_names=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 
  'multi_typing_df.rds'))[['genome_names']])")

alleles=$(Rscript -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR', 
  'multi_typing_df.rds'))[['alleles']])")

rename_file=$(Rscript -e "cat(stringr::str_split_1(readRDS(file.path(
  '$APP_LOCAL_SHARE_DIR', 'multi_typing_df.rds'))[['filenames']], ' '))")

# Remove existing directories
[ -d "$APP_LOCAL_SHARE_DIR/blat_multi" ] && rm -r "$APP_LOCAL_SHARE_DIR/blat_multi"
mkdir "$APP_LOCAL_SHARE_DIR/blat_multi"

results="$APP_LOCAL_SHARE_DIR/blat_multi/results"
[ -d "$results" ] && rm -r "$results"
mkdir "$results"

selected_genomes="$APP_LOCAL_SHARE_DIR/selected_genomes"
log_file="$APP_LOCAL_SHARE_DIR/logs/script_log.txt"

# Create log file or truncate if it exists
echo "Start Multi Typing with $scheme scheme." > "$log_file"

# Remove selected genomes directory
[ -d "$selected_genomes" ] && rm -r "$selected_genomes"
mkdir "$selected_genomes"

file_names=($genome_names)
new_names=($rename_file)

index=0
# Loop through genome files
for file in "${file_names[@]}"; do
  new_file=$(echo "$file" | sed 's/~/ /g')
  
  if [ -f "$genome_folder/$new_file" ]; then
    cp "$genome_folder/$new_file" "$selected_genomes/"
    
    if [ "$new_file" != "${new_names[$index]}.fasta" ]; then
      mv "$selected_genomes/$new_file" \
         "$selected_genomes/${new_names[$index]}.fasta"
    fi
    
    echo "$(date +"%Y-%m-%d %H:%M:%S") - Initiated $new_file" >> "$log_file"
  else
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $new_file not found in $genome_folder" \
         >> "$log_file"
  fi
  index=$((index + 1))
done

# Index genome as database
blat_database="$APP_LOCAL_SHARE_DIR/blat_multi/$scheme"

# Running BLAT loop
for genome in "$selected_genomes"/*; do   
  if ! Rscript "$base_path/bin/check_duplicate_multi.R" "$base_path"; then
    echo "$(date +"%Y-%m-%d %H:%M:%S") - FASTA check failed. Typing of \
      $(basename "$genome") aborted." >> "$log_file"
    continue
  fi

  if [ -f "$genome" ]; then
    genome_filename=$(basename "$genome")
    genome_filename_noext="${genome_filename%.*}"
    echo "$(date +"%Y-%m-%d %H:%M:%S") - Processing $genome_filename" \
         >> "$log_file"
  fi

  mkdir "$results/$genome_filename_noext"
  result_folder="$results/$genome_filename_noext"

  # Run parallelized BLAT
  if ! find "$alleles" -type f \( -name "*.fasta" -o -name "*.fa" -o -name \
    "*.fna" \) | parallel -j $(( $(nproc) - 1 )) pblat "$genome" {} "$result_folder/{/.}.psl" \
    > /dev/null 2>&1; then
    echo "$(date +"%Y-%m-%d %H:%M:%S") - Allele calling failed. Typing of \
      $genome_filename aborted." >> "$log_file"
    continue
  fi

  echo "$(date +"%Y-%m-%d %H:%M:%S") - Attaching $genome_filename" \
       >> "$log_file"

  # Evaluate results
  if ! Rscript "$base_path/bin/multi_eval.R" "$genome_filename"; then
    echo "$(date +"%Y-%m-%d %H:%M:%S") - Results evaluation failed. Typing of \
      $genome_filename aborted." >> "$log_file"
    continue
  fi
done

echo "$(date +"%Y-%m-%d %H:%M:%S") - Multi Typing finalized." >> "$log_file"
