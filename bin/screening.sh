#!/bin/bash

APP_LOCAL_SHARE_DIR="$HOME/.local/share/phylotrace"

# Set base path
base_path=$("$R_HOME/bin/Rscript" -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR',
  'screening_meta.rds'))[,'wd'])")
base_path=${base_path//\'/}

filepaths=$("$R_HOME/bin/Rscript" -e "cat(shQuote(stringr::str_split_1(readRDS(file.path(
  '$APP_LOCAL_SHARE_DIR', 'screening_meta.rds'))[,'selected'], ' ~ ')))")

eval "selected=($filepaths)"

species=$("$R_HOME/bin/Rscript" -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR',
  'screening_meta.rds'))[,'species'])")

database=$("$R_HOME/bin/Rscript" -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR',
  'screening_meta.rds'))[,'database'])")

scheme=$("$R_HOME/bin/Rscript" -e "cat(readRDS(file.path('$APP_LOCAL_SHARE_DIR',
  'screening_meta.rds'))[,'scheme'])")

# Remove the existing directory (if it exists)
if [ -d "$APP_LOCAL_SHARE_DIR/screening" ]; then
  rm -r "$APP_LOCAL_SHARE_DIR/screening"
fi

# Loop through the list of file names and copy them to the new folder
for file in "${selected[@]}"; do
  
  file=${file//\'/}
  
  # Get the directory and base name of the zip file
  zip_dir=$(dirname "$file")
  zip_base=$(basename "$file" .zip)

  unzip -o -j "$file" -d "$zip_dir"
  
  cd "$zip_dir"
  abritamr run --contigs "$zip_base.fasta" --species $species \
     > amrfinder_stdout.txt 2> amrfinder_stderr.txt
  status=$?
  
  mv abritamr/* . && rmdir abritamr
  cd -
  
  # Check exit status 
  if [ "$status" -ne 0 ]; then
    echo "AMRFinder failed with status $status" > "$zip_dir/status.txt"
    echo "Error details:" >> "$zip_dir/status.txt"
    cat amrfinder_stderr.txt >> "$zip_dir/status.txt"
  else
    echo "AMRFinder executed successfully for $zip_base" > "$zip_dir/status.txt"
  fi

  # Clear unzipped assembly
  rm -rf "$zip_dir/$zip_base.fasta"
done

"$R_HOME/bin/Rscript" "$base_path/bin/make_amr_profile.R" "$database" "$scheme" "$base_path"

echo "AMRFinder finalized"
