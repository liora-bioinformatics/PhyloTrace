library(logr)

process_fasta <- function(fasta_path) {
  suppressWarnings({
    # Read the FASTA file into a data.table
    dt <- data.table::fread(fasta_path, header = FALSE, sep = "\n", 
                            col.names = "line", data.table = TRUE)
    
    # Identify headers and sequence lines
    dt[, is_header := grepl("^>", line)]
    
    # Create a group identifier for each sequence based on headers
    dt[, group := cumsum(is_header)]
    
    # Process each group to concatenate sequences, keeping headers as is
    result <- dt[, .(header = line[1], 
                     sequence = paste(line[!is_header], collapse = "")), 
                 by = group]
    
    # Prepare the final output as a character vector
    # Ensure exactly one empty line between sequence end and next header
    output <- unlist(result[, .(output = c(header, sequence, "")), 
                            by = group]$output)
    
    # Remove the last empty line to avoid trailing empty line in the file
    output <- output[-length(output)]
    
    names <- stringr::str_extract(output[seq(1, length(output), by = 3)], 
                                  "^[^\\s]+")
    
    # Test if there are duplicates
    if(length(names) != length(unique(names))){
      
      log_print(paste0("Duplicate(s) present in ", basename(fasta_path)))
      
      # add a number to the duplicates
      for(j in 1:length(names)){
        if(sum(names == names[j]) > 1){
          indices <- which(names == names[j])
          names[j] <- paste0(names[j], "_", which(names == names[j]))
        }
      }
      
      # substitute the respective lines in the file with the new names 
      for(k in 1:length(names)){
        output[3*k - 2] <- paste0(names[k])
      }
      
      # save formatted fasta
      writeLines(output, fasta_path) 
      
      # Return invisible NULL to suppress output
      invisible(NULL)
      
    } else {
      
      # save formatted fasta
      writeLines(output, fasta_path) 
      
      # Return invisible NULL to suppress output
      invisible(NULL)
    }
  })
}

# Get the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Access the first argument
base_path <- args[1]

# Get selected assembly file names
assemblies <- list.files(
  file.path(fs::path_home(), ".local", "share", 
            "phylotrace", "selected_genomes"), full.names = T)

logdir <- file.path(fs::path_home(), ".local", "share", "phylotrace", "logs")
logfile <- file.path(logdir, "check_duplicate_multi.log")

log <- log_open(logfile, logdir = FALSE)

log_print("Initiated multi typing fasta check and formatting")

# Check and format fasta of assemblies
invisible(lapply(assemblies, process_fasta))

log_close()
