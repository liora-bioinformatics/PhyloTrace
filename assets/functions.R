### make_typing_select_handsontable() ----
make_typing_select_handsontable <- function(
  table,
  dupl_mult_id
) {
  # Validate table
  if (is.null(table) || nrow(table) < 1) {
    return(NULL)
  }

  rhandsontable(
    table,
    rowHeaders = NULL,
    stretchH = "all",
    contextMenu = FALSE,
    height = if (nrow(table) > 15) 500 else NULL
  ) %>%
    hot_cols(columnSorting = FALSE) %>%
    hot_rows(rowHeights = 25) %>%
    hot_col(2, readOnly = FALSE, valign = "htBottom") %>%
    hot_cols(2:3, readOnly = TRUE) %>%
    hot_col(1, halign = "htCenter", valign = "htTop", colWidths = 60) %>%
    hot_col(
      7,
      dateFormat = "YYYY-MM-DD",
      type = "date",
      strict = TRUE,
      allowInvalid = TRUE,
      validator = "
                                function (value, callback) {
                                  var today_date = new Date();
                                  today_date.setHours(0, 0, 0, 0);
                                  
                                  var new_date = new Date(value);
                                  new_date.setHours(0, 0, 0, 0);
                                  
                                  try {
                                    if (new_date <= today_date) {
                                      callback(true);
                                      Shiny.setInputValue('invalid_date', false);
                                    } else {
                                      callback(false); 
                                      Shiny.setInputValue('invalid_date', true);
                                    }
                                  } catch (err) {
                                    console.log(err);
                                    callback(false); 
                                    Shiny.setInputValue('invalid_date', true);
                                  }
                                }"
    ) |>
    htmlwidgets::onRender(
      sprintf(
        "function(el, x) {
        var hot = this.hot;
        
        var columnData = hot.getDataAtCol(1); // Change column index if needed
        var duplicates = {};
          
        var highlightInvalidAndDuplicates = function(invalidValues) {
          
          var columnData = hot.getDataAtCol(1); // Change column index if needed
          var duplicates = {};

          // Find all duplicate values
          for (var i = 0; i < columnData.length; i++) {
            var value = columnData[i];
            if (value !== null && value !== undefined) {
              if (duplicates[value]) {
                duplicates[value].push(i);
              } else {
                duplicates[value] = [i];
              }
            }
          }

          // Reset all cell backgrounds in the column
          for (var i = 0; i < columnData.length; i++) {
            var cell = hot.getCell(i, 1); // Change column index if needed
            if (cell) {
              cell.style.background = 'white';
            }
          }

          // Highlight duplicates and invalid values
          for (var i = 0; i < columnData.length; i++) {
            var cell = hot.getCell(i, 1); // Change column index if needed
            var value = columnData[i];
            if (cell) {
              if (invalidValues.includes(value)) {
                cell.style.background = 'rgb(224, 179, 0)'; // Highlight color for invalid values
              } else if (duplicates[value] && duplicates[value].length > 1) {
                cell.style.background = '#FF7334'; // Highlight color for duplicates
              }
            }
          }
        };

        var changefn = function(changes, source) {
          if (source === 'edit' || source === 'undo' || source === 'autofill' || source === 'paste') {
            highlightInvalidAndDuplicates(%s);
          }
        };

        hot.addHook('afterChange', changefn);
        hot.addHook('afterLoadData', function() {
          highlightInvalidAndDuplicates(%s);
        });
        hot.addHook('afterRender', function() {
          highlightInvalidAndDuplicates(%s);
        });

        highlightInvalidAndDuplicates(%s); // Initial highlight on load
        
        Shiny.addCustomMessageHandler('setColumnValue', function(message) {
          var colData = hot.getDataAtCol(0);
          for (var i = 0; i < colData.length; i++) {
            hot.setDataAtCell(i, 0, message.value);
          }
          hot.render(); // Re-render the table
        });
      }",
        jsonlite::toJSON(dupl_mult_id),
        jsonlite::toJSON(dupl_mult_id),
        jsonlite::toJSON(dupl_mult_id),
        jsonlite::toJSON(dupl_mult_id)
      )
    )
}

### generate_rhandsontable() ----
generate_rhandsontable <- function(
  data,
  cust_var,
  compare_select,
  allelic_profile,
  allelic_profile_trunc,
  entry_table_height,
  country_names,
  diff_allele,
  true_rows,
  duplicated_names,
  duplicated_ids,
  err_thresh,
  pinned_entries_highlight
) {
  if (
    !is.null(compare_select) &&
      length(compare_select) > 0 &&
      !is.null(data) &&
      !is.null(cust_var) &&
      !is.null(compare_select)
  ) {
    if (!any((compare_select %in% colnames(allelic_profile)) == FALSE)) {
      entry_data <- data %>%
        select(all_of(1:(14 + nrow(cust_var)))) %>%
        add_column(select(allelic_profile_trunc, any_of(compare_select)))

      col_range <- (15 + nrow(cust_var)):((14 + nrow(cust_var)) +
        length(
          compare_select
        ))
    } else {
      entry_data <- select(data, 1:(14 + nrow(cust_var)))
      col_range <- NULL
    }
  } else {
    if (!is.null(data) && !is.null(cust_var)) {
      entry_data <- select(data, 1:(14 + nrow(cust_var)))
      col_range <- NULL
    }
  }

  if (!is.null(diff_allele)) {
    col_highlight <- diff_allele - 1
  } else {
    col_highlight <- NULL
  }

  if (!is.null(entry_table_height)) {
    height <- entry_table_height
  } else {
    height <- NULL
  }

  if (!is.null(true_rows)) {
    row_highlight <- true_rows - 1
  } else {
    row_highlight <- NULL
  }

  if (!is.null(duplicated_names)) {
    dup_names_high <- duplicated_names - 1
  } else {
    dup_names_high <- NULL
  }

  if (!is.null(duplicated_ids)) {
    dup_ids_high <- duplicated_ids - 1
  } else {
    dup_ids_high <- NULL
  }

  if (!is.null(err_thresh)) {
    error_highlight <- err_thresh - 1
  } else {
    error_highlight <- NULL
  }

  if (!is.null(pinned_entries_highlight)) {
    pinned_highlight <- pinned_entries_highlight - 1
  } else {
    pinned_highlight <- NULL
  }

  args <- list(
    entry_data,
    col_highlight = col_highlight,
    rowHeaders = NULL,
    height = height,
    row_highlight = row_highlight,
    dup_names_high = dup_names_high,
    dup_ids_high = dup_ids_high,
    error_highlight = error_highlight,
    pinned_highlight = pinned_highlight,
    contextMenu = FALSE,
    highlightCol = TRUE,
    highlightRow = TRUE
  )

  tab <- do.call(rhandsontable, args) %>%
    hot_cols(fixedColumnsLeft = 1) %>%
    hot_col(1, valign = "htMiddle", halign = "htCenter") %>%
    hot_col(c(1, 3, 5, 6, 11, 12, 13, 14), readOnly = TRUE) %>%
    hot_col(
      3,
      validator = paste0(
        "function(value, callback) {",
        "try {",
        "if (value === null || value.trim() === '') {",
        "callback(false);",
        "Shiny.setInputValue('empty_id', true);",
        "} else {",
        "callback(true);",
        "Shiny.setInputValue('empty_id', false);",
        "}} catch (err) {",
        "console.log(err);",
        "callback(false);",
        "Shiny.setInputValue('empty_id', true);",
        "}}"
      )
    ) %>%
    hot_col(
      4,
      validator = paste0(
        "function(value, callback) {",
        "try {",
        "if (value === null || value.trim() === '') {",
        "callback(false);",
        "Shiny.setInputValue('empty_name', true);",
        "} else {",
        "callback(true);",
        "Shiny.setInputValue('empty_name', false);",
        "}} catch (err) {",
        "console.log(err);",
        "callback(false);",
        "Shiny.setInputValue('empty_name', true);",
        "}}"
      )
    ) %>%
    hot_col(9, type = "dropdown", source = country_names) %>%
    hot_col(
      7,
      dateFormat = "YYYY-MM-DD",
      type = "date",
      strict = TRUE,
      allowInvalid = TRUE,
      validator = paste0(
        "function (value, callback) {",
        "var today_date = new Date(); today_date.setHours(0, 0, 0, 0);",
        "var new_date = new Date(value); new_date.setHours(0, 0, 0, 0);",
        "try {",
        "if (new_date <= today_date) {",
        "callback(true); Shiny.setInputValue('invalid_date', false);",
        "} else {",
        "callback(false); Shiny.setInputValue('invalid_date', true);",
        "}} catch (err) {",
        "console.log(err);",
        "callback(false); Shiny.setInputValue('invalid_date', true);",
        "}}"
      )
    )

  if (!is.null(cust_var)) {
    tab <- tab %>%
      hot_col(3:(14 + nrow(cust_var)), valign = "htMiddle", halign = "htLeft")
  }

  tab <- tab %>%
    hot_rows(fixedRowsTop = 0) %>%
    hot_col(
      2,
      type = "checkbox",
      width = "auto",
      valign = "htTop",
      halign = "htCenter"
    ) %>%
    hot_col(
      1,
      renderer = paste0(
        "function (instance, td, row, col, prop, value, cellProperties) {",
        "Handsontable.renderers.TextRenderer.apply(this, arguments);",
        "if (instance.params) {",
        "var pinnedRows = instance.params.pinned_highlight;",
        "var highlightedRows = instance.params.row_highlight;",
        "pinnedRows = pinnedRows instanceof Array ? pinnedRows : [pinnedRows];",
        "highlightedRows = highlightedRows instanceof Array ? highlightedRows : [highlightedRows];",
        "if (pinnedRows.includes(row)) {",
        "td.style.backgroundColor = 'orange'; td.style.color = 'white'; }",
        "else if (highlightedRows.includes(row)) {",
        "td.style.backgroundColor = 'rgba(44, 222, 235, 0.6)'; }",
        "}}"
      )
    ) %>%
    hot_col(
      13,
      renderer = paste0(
        "function (instance, td, row, col, prop, value, cellProperties) {",
        "Handsontable.renderers.TextRenderer.apply(this, arguments);",
        "if (instance.params) {",
        "hrows = instance.params.error_highlight;",
        "hrows = hrows instanceof Array ? hrows : [hrows];",
        "if (hrows.includes(row)) { ",
        "td.style.backgroundColor = 'rgba(255, 80, 1, 0.8)'; }",
        "}}"
      )
    ) %>%
    hot_col(
      4,
      renderer = paste0(
        "function (instance, td, row, col, prop, value, cellProperties) {",
        "Handsontable.renderers.TextRenderer.apply(this, arguments);",
        "if (instance.params) {",
        "hrows = instance.params.dup_names_high;",
        "hrows = hrows instanceof Array ? hrows : [hrows];",
        "if (hrows.includes(row)) {",
        "td.style.backgroundColor = 'rgb(224, 179, 0)'; }",
        "}}"
      )
    ) %>%
    hot_col(
      3,
      renderer = paste0(
        "function (instance, td, row, col, prop, value, cellProperties) {",
        "Handsontable.renderers.TextRenderer.apply(this, arguments);",
        "if (instance.params) {",
        "hrows = instance.params.dup_ids_high;",
        "hrows = hrows instanceof Array ? hrows : [hrows];",
        "if (hrows.includes(row)) {",
        "td.style.backgroundColor = 'rgb(224, 179, 0)'; }",
        "}}"
      )
    )

  if (!is.null(diff_allele)) {
    tab <- tab %>%
      hot_col(
        diff_allele,
        renderer = paste0(
          "function(instance, td, row, col, prop, value, cellProperties) {",
          "Handsontable.renderers.NumericRenderer.apply(this, arguments);",
          "if (instance.params) {",
          "hcols = instance.params.col_highlight;",
          "hcols = hcols instanceof Array ? hcols : [hcols];",
          "if (hcols.includes(col)) {",
          "td.style.background = 'rgb(116, 188, 139)'; }",
          "}}"
        )
      )
  }

  if (!is.null(col_range)) {
    tab <- tab %>%
      hot_col(
        col_range,
        readOnly = TRUE,
        valign = "htMiddle",
        halign = "htCenter"
      )
  }

  return(tab)
}


merge_and_fix_types <- function(df1, df2) {
  # Identify common columns
  common_cols <- intersect(names(df1), names(df2))

  # Convert all logical columns in df2 to character
  df2[] <- lapply(df2, function(col) {
    if (is.logical(col)) {
      return(as.character(col)) # Convert logical to character
    }
    return(col)
  })

  # List to store column names that need renaming
  cols_to_rename <- list()

  # Check for type mismatches in common columns
  for (col in common_cols) {
    class1 <- class(df1[[col]])
    class2 <- class(df2[[col]])

    if (!identical(class1, class2)) {
      # Attempt to convert df2's column to df1's type
      converted <- tryCatch(
        {
          if (is.numeric(df1[[col]])) {
            suppressWarnings(as.numeric(df2[[col]])) # Avoid warnings
          } else if (is.character(df1[[col]])) {
            as.character(df2[[col]])
          } else {
            stop("Unsupported type")
          }
        },
        warning = function(w) NULL,
        error = function(e) NULL
      )

      # If conversion fails or logical/numeric mismatch, move to new column
      if (
        is.null(converted) ||
          any(is.na(converted) & !is.na(df2[[col]])) ||
          (is.logical(df1[[col]]) && is.numeric(df2[[col]])) ||
          (is.numeric(df1[[col]]) && is.logical(df2[[col]]))
      ) {
        cols_to_rename[[col]] <- paste0(col, "_ext")
      } else {
        df2[[col]] <- converted # Apply successful conversion
      }
    }
  }

  # Rename columns in df2 that couldn't be converted
  for (col in names(cols_to_rename)) {
    new_col_name <- cols_to_rename[[col]]
    names(df2)[names(df2) == col] <- new_col_name
    message(paste(
      "Column",
      col,
      "in df2 couldn't be converted and is renamed to",
      new_col_name
    ))
  }

  # Get all unique column names after renaming
  all_columns <- union(names(df1), names(df2))

  # Ensure both data frames have all necessary columns
  for (col in all_columns) {
    if (!col %in% names(df1)) {
      df1[[col]] <- NA
    } # Add missing column to df1
    if (!col %in% names(df2)) df2[[col]] <- NA # Add missing column to df2
  }

  # Keep df1’s column order and append any new columns from df2
  ordered_cols <- c(names(df1), setdiff(names(df2), names(df1)))
  df1 <- df1[, ordered_cols, drop = FALSE]
  df2 <- df2[, ordered_cols, drop = FALSE]

  # Append rows from df2 below df1
  merged_df <- rbind(df1, df2)

  merged_df[] <- lapply(merged_df, function(col) {
    if (is.character(col)) {
      col[is.na(col)] <- ""
    }
    if (is.numeric(col)) {
      col[is.na(col)] <- 0
    }
    return(col)
  })

  return(merged_df)
}

help_func <- function(x) {
  if (is.character(x) && is.na(x)) {
    x <- ""
  }
  if (is.numeric(x) && is.na(x)) {
    x <- 0
  }
  return(x)
}

# helper function to replace .is_not
is_not <- function(x) {
  is.null(x) || identical(x, FALSE)
}

# map hashes to allele profile
hash_allele_profile <- function(allele_profile, hashed_loci_folder) {
  # List all hashed loci files
  all_loci_files <- list.files(hashed_loci_folder, full.names = TRUE)

  hashed_data <- list()

  for (file_path in all_loci_files) {
    # Extract locus name
    locus_name <- sub("\\.(fasta|fa|fna)$", "", basename(file_path))
    # Store file content
    hashed_data[[locus_name]] <- readLines(file_path)
  }

  # Convert allele_profile to data.table
  allele_profile_hashed <- as.data.table(allele_profile)

  # Process each column (locus)
  for (locus_name in colnames(allele_profile_hashed)) {
    if (!locus_name %in% names(hashed_data)) {
      next
    } # Skip if locus not found

    variants <- hashed_data[[locus_name]]
    hashed_index <- gsub(
      ">",
      "",
      variants[seq(1, length(variants) - 1, by = 3)]
    ) # Extract hashes

    allele_profile_hashed[[locus_name]] <- sapply(
      allele_profile_hashed[[locus_name]],
      function(varnum) {
        if (is.na(varnum) || varnum > length(hashed_index)) {
          return(NA)
        }
        hashed_index[varnum]
      }
    )
  }

  return(as.data.frame(allele_profile_hashed))
}


# Truncate strings longer than 37 characters
truncate_start <- function(text, max_length = 37, keep_start = 10) {
  if (nchar(text) <= max_length) {
    return(text)
  }

  # Calculate how many characters to keep at the end
  keep_end <- max_length - keep_start - 3

  # Construct truncated text
  paste0(
    substr(text, 1, keep_start),
    "...",
    substr(text, nchar(text) - keep_end + 1, nchar(text))
  )
}

# Truncate strings longer than 22 characters at end
truncate_if_long <- function(x, max_length = 22) {
  if (nchar(x) > max_length) {
    return(paste0(substr(x, 1, max_length), "..."))
  }
  return(x)
}

# check allelic profile import index type
is_integer_vector <- function(vec) {
  if (!is.vector(vec)) {
    return(FALSE)
  } # Ensure input is a vector

  # Try converting to numeric
  numeric_version <- suppressWarnings(as.numeric(vec))

  # Ignore NA values and check if the remaining values are whole numbers
  valid_values <- numeric_version[!is.na(numeric_version)]

  # Check if all non-NA values are whole numbers (integers)
  return(all(valid_values == floor(valid_values)))
}

# detect import file delimiter type
detect_delimiter <- function(file_path) {
  first_lines <- readLines(file_path, n = 5)

  # Define possible delimiters
  delimiters <- c(
    "," = ",",
    ";" = ";",
    "\t" = "tab",
    "|" = "pipe",
    ":" = "colon"
  )

  # Count occurrences of each delimiter
  counts <- sapply(names(delimiters), function(d) sum(grepl(d, first_lines)))

  # Pick the most frequent delimiter
  best_delimiter <- names(which.max(counts))

  # Return the best match
  return(best_delimiter)
}

# Helper function for determining scale choices
determine_scale_choices <- function(
  meta_nj,
  variable_val,
  gradient_scales,
  diverging_scales,
  qualitative_scales,
  sequential_scales
) {
  if (!is.null(meta_nj)) {
    variable_data <- unlist(meta_nj[variable_val])
    if (class(variable_data) == "numeric") {
      list(Continuous = gradient_scales, Diverging = diverging_scales)
    } else if (length(unique(variable_data)) > 7) {
      list(Gradient = gradient_scales)
    } else {
      list(Qualitative = qualitative_scales, Sequential = sequential_scales)
    }
  } else {
    list(Qualitative = qualitative_scales, Sequential = sequential_scales)
  }
}

# Helper function for determining the scale value
determine_scale <- function(
  variable_val,
  meta_nj,
  numeric_scale = "viridis",
  long_scale = "turbo",
  short_scale = "Accent"
) {
  if (!is.null(variable_val) && !is.null(meta_nj)) {
    if (class(unlist(meta_nj[variable_val])) == "numeric") {
      numeric_scale
    } else if (length(unique(unlist(meta_nj[variable_val]))) > 7) {
      long_scale
    } else {
      short_scale
    }
  } else {
    numeric_scale
  }
}

# Render reactive plot control input
render_plot_control <- function(
  input_id,
  input_type,
  label = "",
  choices = NA,
  reactive_value,
  min = 0,
  max = 100,
  default_value,
  reset = FALSE,
  width = "100%",
  show_condition = TRUE,
  div_class = "",
  right = TRUE,
  ticks = FALSE,
  step = 1,
  options = NA,
  multiple = FALSE
) {
  # Determine the selected value based on reset condition
  if (is.null(reactive_value)) {
    sel <- NULL
  } else {
    if (isTRUE(reset)) {
      sel <- default_value
    } else {
      sel <- reactive_value
    }
  }

  # Prepare the initial arguments list
  args <- list(
    inputId = input_id,
    label = label,
    choices = choices,
    selected = sel,
    width = width,
    right = right,
    value = sel,
    min = min,
    max = max,
    step = step,
    ticks = ticks,
    options = options,
    multiple = multiple
  )

  # Get the formal arguments of the specified input_type function
  formal_args <- names(formals(input_type))

  # Filter the args list to include only those valid for the input_type function
  filtered_args <- args[names(args) %in% formal_args]

  # Use do.call to dynamically call the input_type function with filtered arguments
  output <- do.call(input_type, filtered_args)

  # Wrap the output in a div or shinyjs::disabled conditionally
  if (length(div_class) > 0) {
    div(
      class = div_class,
      if (show_condition) output else shinyjs::disabled(output)
    )
  } else {
    if (show_condition) output else shinyjs::disabled(output)
  }
}

# function to fetch latest urls
get_latest_url <- function(abb) {
  response <- httr::GET("https://www.cgmlst.org/ncs/schema/")

  if (response$status_code == 200) {
    page_content <- read_html(
      httr::content(response, as = "text", encoding = "utf-8"),
      "utf-8"
    )

    links <- page_content %>%
      html_nodes("a") %>%
      html_attr("href")

    pattern <- paste(abb, collapse = "|")

    filtered_urls <- links[grep(pattern, links)]

    # Neuesten Link basierend auf der Suffix-Zahl finden
    if (length(filtered_urls) > 0) {
      return(filtered_urls)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# Reset reactive variables
resetVars <- function(varList) {
  for (name in names(varList)) {
    varList[[name]] <- NULL
  }
}

# Function to read and format FASTA sequences
format_fasta <- function(filepath) {
  if (!file.exists(filepath)) {
    return()
  }
  fasta <- readLines(filepath)
  formatted_fasta <- list()
  current_sequence <- ""

  for (line in fasta) {
    if (startsWith(line, ">")) {
      if (current_sequence != "") {
        formatted_fasta <- append(formatted_fasta, list(current_sequence))
        current_sequence <- ""
      }
      formatted_fasta <- append(formatted_fasta, list(line))
    } else {
      current_sequence <- paste0(current_sequence, line)
    }
  }
  if (current_sequence != "") {
    formatted_fasta <- append(formatted_fasta, list(current_sequence))
  }

  formatted_fasta
}

# Function to color-code the bases in a sequence
color_sequence <- function(sequence) {
  sequence <- gsub("A", "<span class='base-a'>A</span>", sequence)
  sequence <- gsub("T", "<span class='base-t'>T</span>", sequence)
  sequence <- gsub("G", "<span class='base-g'>G</span>", sequence)
  sequence <- gsub("C", "<span class='base-c'>C</span>", sequence)
  sequence
}

# Function to log messages to logfile
log_message <- function(log_file, message, append = TRUE) {
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "-",
    message,
    "\n",
    file = log_file,
    append = append
  )
}

# Modified gheatmap function
gheatmap.mod <- function(
  p,
  data,
  offset = 0,
  width = 1,
  low = "green",
  high = "red",
  color = "white",
  colnames = TRUE,
  colnames_position = "bottom",
  colnames_angle = 0,
  colnames_level = NULL,
  colnames_offset_x = 0,
  colnames_offset_y = 0,
  font.size = 4,
  family = "",
  hjust = 0.5,
  legend_title = "value",
  colnames_color = "black"
) {
  colnames_position %<>% match.arg(c("bottom", "top"))
  variable <- value <- lab <- y <- NULL

  ## if (is.null(width)) {
  ##     width <- (p$data$x %>% range %>% diff)/30
  ## }

  ## convert width to width of each cell
  width <- width * (p$data$x %>% range(na.rm = TRUE) %>% diff) / ncol(data)

  isTip <- x <- y <- variable <- value <- from <- to <- NULL

  ## handle the display of heatmap on collapsed nodes
  ## https://github.com/GuangchuangYu/ggtree/issues/242
  ## extract data on leaves (& on collapsed internal nodes)
  ## (the latter is extracted only when the input data has data on collapsed
  ## internal nodes)
  df <- p$data
  nodeCo <- intersect(
    df %>% filter(is.na(x)) %>% select(.data$parent, .data$node) %>% unlist(),
    df %>% filter(!is.na(x)) %>% select(.data$parent, .data$node) %>% unlist()
  )
  labCo <- df %>%
    filter(.data$node %in% nodeCo) %>%
    select(.data$label) %>%
    unlist()
  selCo <- intersect(labCo, rownames(data))
  isSel <- df$label %in% selCo

  df <- df[df$isTip | isSel, ]
  start <- max(df$x, na.rm = TRUE) + offset

  dd <- as.data.frame(data)
  ## dd$lab <- rownames(dd)
  i <- order(df$y)

  ## handle collapsed tree
  ## https://github.com/GuangchuangYu/ggtree/issues/137
  i <- i[!is.na(df$y[i])]

  lab <- df$label[i]
  ## dd <- dd[lab, , drop = FALSE]
  ## https://github.com/GuangchuangYu/ggtree/issues/182
  dd <- dd[match(lab, rownames(dd)), , drop = FALSE]

  dd$y <- sort(df$y)
  dd$lab <- lab
  ## dd <- melt(dd, id = c("lab", "y"))
  dd <- gather(dd, variable, value, -c(lab, y))

  i <- which(dd$value == "")
  if (length(i) > 0) {
    dd$value[i] <- NA
  }
  if (is.null(colnames_level)) {
    dd$variable <- factor(dd$variable, levels = colnames(data))
  } else {
    dd$variable <- factor(dd$variable, levels = colnames_level)
  }
  V2 <- start + as.numeric(dd$variable) * width
  mapping <- data.frame(from = dd$variable, to = V2)
  mapping <- unique(mapping)

  dd$x <- V2
  dd$width <- width
  dd[[".panel"]] <- factor("Tree")
  if (is.null(color)) {
    p2 <- p +
      geom_tile(
        data = dd,
        aes(x, y, fill = value),
        width = width,
        inherit.aes = FALSE
      )
  } else {
    p2 <- p +
      geom_tile(
        data = dd,
        aes(x, y, fill = value),
        width = width,
        color = color,
        inherit.aes = FALSE
      )
  }
  if (is(dd$value, "numeric")) {
    p2 <- p2 +
      scale_fill_gradient(
        low = low,
        high = high,
        na.value = NA,
        name = legend_title
      ) # "white")
  } else {
    p2 <- p2 + scale_fill_discrete(na.value = NA, name = legend_title) #"white")
  }

  if (colnames) {
    if (colnames_position == "bottom") {
      y <- 0
    } else {
      y <- max(p$data$y) + 1
    }
    mapping$y <- y
    mapping[[".panel"]] <- factor("Tree")
    p2 <- p2 +
      geom_text(
        data = mapping,
        aes(x = to, y = y, label = from),
        color = colnames_color,
        size = font.size,
        family = family,
        inherit.aes = FALSE,
        angle = colnames_angle,
        nudge_x = colnames_offset_x,
        nudge_y = colnames_offset_y,
        hjust = hjust
      )
  }

  p2 <- p2 + theme(legend.position = "right")
  ## p2 <- p2 + guides(fill = guide_legend(override.aes = list(colour = NULL)))

  if (!colnames) {
    ## https://github.com/GuangchuangYu/ggtree/issues/204
    p2 <- p2 + scale_y_continuous(expand = c(0, 0))
  }

  attr(p2, "mapping") <- mapping
  return(p2)
}

# Function to find columns with varying values
var_alleles <- function(dataframe) {
  varying_columns <- c()

  for (col in 1:ncol(dataframe)) {
    unique_values <- unique(dataframe[, col])

    if (length(unique_values) > 1) {
      varying_columns <- c(varying_columns, col)
    }
  }

  return(varying_columns)
}

# Functions to compute hamming distances dependent on missing value handling
hamming.dist <- function(x, y) {
  sum(x != y)
}

hamming.distIgnore <- function(x, y) {
  sum((x != y) & !is.na(x) & !is.na(y))
}

hamming.distCategory <- function(x, y) {
  sum((x != y | xor(is.na(x), is.na(y))) & !(is.na(x) & is.na(y)))
}

compute.distMatrix <- function(profile, hamming.method) {
  mat <- as.matrix(profile)
  n <- nrow(mat)
  dist_mat <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dist_mat[i, j] <- hamming.method(x = mat[i, ], y = mat[j, ])
      dist_mat[j, i] <- dist_mat[i, j]
    }
  }
  return(dist_mat)
}

# Create Hamming distance matrix
hamming_dist_matrix <- function(x) {
  # Number of observations
  n <- nrow(x)

  # Initialize distance matrix
  dist_mat <- matrix(0, nrow = n, ncol = n)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Calculate Hamming distance
      hamming_dist <- sum(x[i, ] != x[j, ]) / ncol(x)
      dist_mat[i, j] <- dist_mat[j, i] <- hamming_dist
    }
  }

  # Convert to dist object
  return(as.dist(dist_mat))
}

# Create Matthews correlation distance matrix
mcc_dist_matrix <- function(x) {
  # Number of observations
  n <- nrow(x)

  # Initialize distance matrix
  dist_mat <- matrix(0, nrow = n, ncol = n)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Get vectors for comparison
      vec1 <- x[i, ]
      vec2 <- x[j, ]

      # Calculate contingency table elements
      tp <- sum(vec1 == 1 & vec2 == 1) # true positives
      tn <- sum(vec1 == 0 & vec2 == 0) # true negatives
      fp <- sum(vec1 == 0 & vec2 == 1) # false positives
      fn <- sum(vec1 == 1 & vec2 == 0) # false negatives

      # Calculate MCC
      denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

      if (denominator == 0) {
        mcc <- 0 # Handle division by zero
      } else {
        mcc <- (tp * tn - fp * fn) / denominator
      }

      # Normalize for [0,1] range
      dist_mat[i, j] <- dist_mat[j, i] <- (1 - mcc) / 2
    }
  }

  # Convert to dist object
  return(as.dist(dist_mat))
}

# Function to check custom variable classes
column_classes <- function(df) {
  sapply(df, function(x) {
    if (class(x) == "numeric") {
      return("cont")
    } else if (class(x) == "character") {
      return("categ")
    } else {
      return(class(x))
    }
  })
}

# Function to hash database
hash_database <- function(folder, filtered = FALSE, progress = NULL) {
  if (isFALSE(filtered)) {
    loci_files <- list.files(folder)
  } else {
    loci_files <- list.files(folder)[filtered]
  }

  loci_names <- sapply(strsplit(loci_files, "[.]"), function(x) x[1])
  loci_paths <- file.path(folder, loci_files)
  count <- length(loci_files)
  hashes <- sapply(loci_paths, hash_locus, progress = progress, count = count)

  names(hashes) <- loci_names
  hashes
}

# Function to hash a locus
hash_locus <- function(locus_path, progress, count) {
  locus_file <- readLines(locus_path)
  if (length(locus_file) > 0) {
    seq_list <- locus_file[seq(2, length(locus_file), 3)]
    seq_hash <- sha256(seq_list)
    seq_idx <- paste0(">", seq_hash)

    locus_file[seq(1, length(locus_file), 3)] <- seq_idx
    writeLines(locus_file, locus_path)
    if (!is.null(progress) & !is.null(count)) {
      progress$inc(
        1 / count,
        detail = paste("Hashed", basename(locus_path)),
        message = "Hashing alleles"
      )
    }
    seq_hash
  }
}

# Get locus hashes
get_locus_hashes <- function(locus_path) {
  locus_file <- readLines(locus_path)
  hash_list <- locus_file[seq(1, length(locus_file), 3)]
  hash_list <- sapply(strsplit(hash_list, "[>]"), function(x) x[2])
}

extract_seq <- function(locus_path, hashes) {
  locus_file <- readLines(locus_path)
  hash_list <- sapply(
    strsplit(locus_file[seq(1, length(locus_file), 3)], "[>]"),
    function(x) x[2]
  )
  seq_list <- locus_file[seq(2, length(locus_file), 3)]
  seq_idx <- hash_list %in% hashes

  list(
    idx = hash_list[seq_idx],
    seq = seq_list[seq_idx]
  )
}

add_new_sequences <- function(locus_path, sequences, progress) {
  locus_file <- file(locus_path, open = "a+")
  for (i in seq_along(sequences$idx)) {
    writeLines(
      c("", paste0(">", sequences$idx[i]), sequences$seq[i]),
      locus_file
    )
  }
  close(locus_file)
}

# Compute clusters to use in visNetwork
compute_clusters <- function(nodes, edges, threshold) {
  groups <- rep(0, length(nodes$id))
  edge_group <- rep(0, length(edges$from))

  edges_table <- data.frame(
    from = edges$from,
    to = edges$to,
    weight = edges$weight
  )

  count <- 0
  while (any(groups == 0)) {
    group_na <- groups == 0
    labels <- nodes$id[group_na]

    cluster <- nodes$id[group_na][1] # Initialize with 1 label
    while (!is_empty(labels)) {
      sub_tb <- edges_table[
        (edges_table$from %in% cluster | edges_table$to %in% cluster) &
          edges_table$weight <= threshold,
      ]

      if (
        nrow(sub_tb) == 0 |
          length(unique(c(sub_tb$from, sub_tb$to))) == length(cluster)
      ) {
        count <- count + 1
        groups[nodes$id %in% cluster] <- paste("Group", count)
        edge_group[edges$from %in% cluster & edges$to %in% cluster] <- paste(
          "Group",
          count
        )
        break
      } else {
        cluster <- unique(c(sub_tb$from, sub_tb$to))
      }
    }
  }
  list(groups = groups, edge_group = edge_group)
}

# Check gene screening status
check_status <- function(isolate, database, scheme) {
  iso_name <- gsub(".zip", "", basename(isolate))
  if (
    file.exists(file.path(
      database,
      gsub(" ", "_", scheme),
      "Isolates",
      iso_name,
      "status.txt"
    ))
  ) {
    if (
      str_detect(
        readLines(file.path(
          database,
          gsub(" ", "_", scheme),
          "Isolates",
          iso_name,
          "status.txt"
        ))[1],
        "successfully"
      )
    ) {
      return("success")
    } else {
      return("fail")
    }
  } else {
    return("unfinished")
  }
}

# Reset gene screening status
remove.screening.status <- function(isolate, database, scheme) {
  if (
    file.exists(file.path(
      database,
      gsub(" ", "_", scheme),
      "Isolates",
      isolate,
      "status.txt"
    ))
  ) {
    file.remove(file.path(
      database,
      gsub(" ", "_", scheme),
      "Isolates",
      isolate,
      "status.txt"
    ))
  }
}

# Truncate hashes
truncHash <- function(hash) {
  if (!is.na(hash)) {
    paste0(
      str_sub(hash, 1, 4),
      "...",
      str_sub(
        hash,
        nchar(hash) - 3,
        nchar(hash)
      )
    )
  } else {
    NA
  }
}

# Function to check for HTTP errors and parse content
safe.api.call <- function(endpoint) {
  response <- tryCatch(
    httr::GET(endpoint),
    error = function(e) {
      message("Error in API call: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (httr::http_error(response)) {
    message("HTTP error: ", httr::status_code(response))
    return(NULL)
  }

  content <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) {
      message("Error in parsing response content: ", e$message)
      return(NULL)
    }
  )

  if (is.null(content)) {
    return(NULL)
  }

  parsed_content <- tryCatch(
    jsonlite::fromJSON(content, flatten = TRUE),
    error = function(e) {
      message("Error in parsing JSON content: ", e$message)
      return(NULL)
    }
  )

  return(parsed_content)
}

parse.names <- function(species) {
  if (species == "Borrelia spp") {
    return("Borrelia")
  } else if (species == "Brucella spp") {
    return("Brucella")
  } else if (
    species == "Burkholderia mallei FLI" |
      species == "Burkholderia mallei RKI"
  ) {
    return("Burkholderia mallei")
  } else if (
    species == "Campylobacter jejuni coli v1" |
      species == "Campylobacter jejuni coli v2" |
      species == "Campylobacter jejuni coli"
  ) {
    return(c("Campylobacter jejuni", "Campylobacter coli"))
  } else if (species == "Cronobacter sakazakii malonaticus") {
    return(c("Cronobacter sakazakii", "Cronobacter malonaticus"))
  } else if (species == "Escherichia spp") {
    return("Escherichia")
  } else if (
    species == "Human-restricted Neisseria v1" |
      species == "Neisseria L3" |
      species == "Neisseria L44"
  ) {
    return("Neisseria")
  } else if (
    species == "Neisseria gonorrhoeae v1" |
      species == "Neisseria gonorrhoeae v2"
  ) {
    return("Neisseria gonorrhoeae")
  } else if (
    species == "Neisseria meningitidis v1" |
      species == "Neisseria meningitidis v2" |
      species == "Neisseria meningitidis v3"
  ) {
    return("Neisseria meningitidis")
  } else if (
    species == "Salmonella v1" |
      species == "Salmonella v2 enterobase"
  ) {
    return("Salmonella")
  } else if (species == "Klebsiella oxytoca sensu lato") {
    return(paste(
      "Klebsiella",
      c("oxytoca", "grimontii", "michiganensis", "pasteurii")
    ))
  } else if (species == "Klebsiella pneumoniae sensu lato") {
    return(paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae")))
  } else if (species == "Mycobacterium tuberculosis complex") {
    return(paste(
      "Mycobacterium",
      c("tuberculosis", "tuberculosis variant bovis", "canetti")
    ))
  } else if (species == "Serratia spp") {
    return("Serratia")
  } else if (species == "Leptospira spp") {
    return("Leptospira")
  } else if (species == "Mycobacteroides abscessus complex") {
    return("Mycobacteroides abscessus")
  } else if (species == "Citrobacter sensu lato") {
    return(paste(
      "Citrobacter",
      c("freundii", "portucalensis", "braakii", "europaeus")
    ))
  } else {
    return(species)
  }
}

check.amrfinder.available <- function(selected_scheme, amrfinder_species) {
  scheme_species <- gsub(" (CM|PM)", "", gsub("_", " ", selected_scheme))

  parsed_species <- parse.names(scheme_species)

  if (length(parsed_species) == 1) {
    # Exceptions
    if (
      parsed_species == "Burkholderia mallei FLI" |
        parsed_species == "Burkholderia mallei RKI"
    ) {
      parsed_species <- "Burkholderia mallei"
    } else if (parsed_species == "Escherichia coli") {
      parsed_species <- "Escherichia"
    } else if (parsed_species == "Salmonella enterica") {
      parsed_species <- "Salmonella"
    }

    return(ifelse(
      any(parsed_species == gsub("_", " ", amrfinder_species)),
      parsed_species,
      FALSE
    ))
  } else {
    # Exceptions
    if (
      identical(
        parsed_species,
        paste(
          "Klebsiella",
          c("oxytoca", "grimontii", "michiganensis", "pasteurii")
        )
      )
    ) {
      parsed_species <- "Klebsiella oxytoca"
    } else if (
      identical(
        parsed_species,
        paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae"))
      )
    ) {
      parsed_species <- "Klebsiella pneumoniae"
    } else if (
      identical(
        parsed_species,
        paste("Klebsiella", c("pneumoniae", "variicola", "quasipneumoniae"))
      )
    ) {
      parsed_species <- "Klebsiella pneumoniae"
    } else if (
      identical(parsed_species, paste("Campylobacter", c("jejuni", "coli")))
    ) {
      parsed_species <- "Campylobacter"
    }

    return(ifelse(
      any(parsed_species == gsub("_", " ", amrfinder_species)),
      parsed_species,
      FALSE
    ))
  }
}

# Function to fetch species data from NCBI
fetch.species.data <- function(species) {
  parsed_species <- parse.names(species)

  multiple <- list()
  for (i in seq_along(parsed_species)) {
    # Serratia exception
    ifelse(
      parsed_species[i] == "Serratia",
      command <- paste0("datasets summary taxonomy taxon '", 613, "'"),
      command <- paste0(
        "datasets summary taxonomy taxon '",
        parsed_species[i],
        "'"
      )
    )

    tryCatch(
      result <- system(command, intern = TRUE),
      error = function(e) {
        message("Error in NCBI Datasets call: ", e$message)
        return(NULL)
      }
    )

    if (is.null(result)) {
      message(paste("Error: ", parsed_species, " not available on NCBI."))
      return(NULL)
    } else {
      tryCatch(
        content <- jsonlite::fromJSON(paste(result, collapse = "")),
        error = function(e) {
          message("Error in NCBI Datasets call: ", e$message)
          return(NULL)
        }
      )

      if (!is.null(content$reports)) {
        species_data <- content$reports$taxonomy

        message("Fetched taxonomy for ", species)

        multiple[[gsub(" ", "_", parsed_species[i])]] <- list(
          Name = species_data$current_scientific_name,
          ID = species_data$tax_id,
          Classification = species_data$classification,
          Group = species_data$group_name,
          Image = paste0(
            "https://api.ncbi.nlm.nih.gov/datasets/v2alpha/taxonomy/taxon/",
            species_data$tax_id,
            "/image"
          )
        )
      } else {
        message("Empty taxonomy data")
        return(NULL)
      }
    }
  }

  if (!is.null(multiple)) {
    return(multiple)
  } else {
    message("Empty taxonomy data")
    return(NULL)
  }
}

# Function to retrieve cgmlst scheme information
get.schemeinfo <- function(url_link) {
  endpoint <- paste0(url_link, "?return_all=1")
  scheme_info <- safe.api.call(endpoint)

  if (is.null(scheme_info)) {
    message("Failed to retrieve scheme information.")
    return(NULL)
  }

  return(scheme_info)
}

parse.schemeinfo <- function(
  scheme_info,
  repo = "PM",
  database,
  folder_name,
  url_link
) {
  if (repo == "PM") {
    # Get curators
    if (!is.null(scheme_info[["curators"]])) {
      curators <- character()
      for (i in 1:length(scheme_info[["curators"]])) {
        curators_list <- get.schemeinfo(scheme_info[["curators"]][i])
        curator <- paste(
          curators_list$first_name,
          curators_list$surname,
          "<br>\n",
          curators_list$affiliation
        )

        curators <- c(curators, curator)

        # Small delay before next request
        if (length(scheme_info[["curators"]]) > 1) {
          Sys.sleep(0.5)
        }
      }

      curators <- paste(curators, collapse = "<br>\n<br>\n")
    } else {
      curators <- "Not Available"
    }

    # Get flags
    if (!is.null(scheme_info[["flags"]])) {
      flags <- paste(scheme_info[["flags"]], collapse = "<br>\n")
    } else {
      flags <- "Not Available"
    }

    # Get schemem description
    if (!is.null(scheme_info[["description"]])) {
      description <- scheme_info[["description"]]
    } else {
      description <- "Not Available"
    }

    # Get last update info
    if (!is.null(scheme_info[["last_updated"]])) {
      last_updated <- scheme_info[["last_updated"]]
    } else {
      last_updated <- "Not Available"
    }

    # Get locus count
    if (!is.null(scheme_info[["last_updated"]])) {
      last_updated <- scheme_info[["last_updated"]]
    } else {
      last_updated <- "Not Available"
    }

    scheme_overview <- data.frame(
      x1 = c(
        "Scheme",
        "Database",
        "Repository",
        "Version",
        "Locus Count",
        "Curators",
        "Last Change",
        "Flags"
      ),
      x2 = c(
        gsub("_", " ", folder_name),
        "pubMLST",
        paste0(
          '<a href="',
          paste0(
            "https://www.pubmlst.org/bigsdb?db=",
            basename(dirname(dirname(url_link)))
          ),
          '" target="_blank">',
          paste0(
            "https://www.pubmlst.org/bigsdb?db=",
            basename(dirname(dirname(url_link)))
          ),
          '</a>'
        ),
        description,
        scheme_info[["locus_count"]],
        curators,
        last_updated,
        flags
      )
    )

    names(scheme_overview) <- NULL

    return(scheme_overview)
  }
}

# Function to download all alleles of each loci of selected scheme
download.alleles.PM <- function(url_link, database, folder_name, progress) {
  # Make scheme directory
  directory <- file.path(database, folder_name)
  if (!dir.exists(directory)) {
    tryCatch(
      {
        dir.create(directory, recursive = TRUE)
        message("Directory created: ", directory)
      },
      error = function(e) {
        stop("Failed to create directory: ", directory, "\nError: ", e$message)
      }
    )
  }

  # retrieve and save scheme info
  scheme_info <- get.schemeinfo(url_link)
  if (is.null(scheme_info$loci) || length(scheme_info$loci) == 0) {
    stop("No loci found in scheme_info.")
  } else {
    scheme_overview <- parse.schemeinfo(
      scheme_info = scheme_info,
      repo = "PM",
      database = database,
      folder_name = folder_name,
      url_link = url_link
    )

    saveRDS(scheme_overview, file.path(directory, "scheme_info.rds"))

    message("Scheme info downloaded")
  }

  ### Download alleles

  # Make output folder
  output_folder <- file.path(
    database,
    ".downloaded_schemes",
    paste0(folder_name)
  )

  if (!dir.exists(output_folder)) {
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop(
          "Failed to create directory: ",
          output_folder,
          "\nError: ",
          e$message
        )
      }
    )
  } else {
    unlink(output_folder, recursive = TRUE, force = TRUE)
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop(
          "Failed to create directory: ",
          output_folder,
          "\nError: ",
          e$message
        )
      }
    )
  }

  # Initialize vector to store the paths of downloaded files
  downloaded_files <- vector("character", length(scheme_info$loci))

  for (i in seq_along(scheme_info$loci)) {
    locus_url <- scheme_info$loci[i]
    endpoint <- paste0(locus_url, "/alleles_fasta?return_all=1")

    response <- httr::GET(endpoint)

    if (httr::http_error(response)) {
      stop("Failed to retrieve data for locus: ", basename(locus_url))
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")

    if (is.null(content)) {
      stop("Failed to parse content for locus: ", basename(locus_url))
    }

    # Insert an empty line between sequences
    formatted_content <- gsub("\n>", "\n\n>", content)

    # Write to file
    output_file <- file.path(
      output_folder,
      paste0(basename(locus_url), ".fasta")
    )

    writeLines(formatted_content, con = output_file)

    downloaded_files[i] <- output_file

    # Increment the progress bar
    progress$inc(
      1 / (2 * length(seq_along(scheme_info$loci))),
      detail = paste("Saved", basename(locus_url))
    )

    message("Saved fasta file for locus: ", basename(locus_url))
  }

  # Zip folder
  system(paste0(
    "zip -r -j ",
    shQuote(output_folder),
    ".zip ",
    shQuote(output_folder),
    "/"
  ))
  unlink(output_folder, recursive = TRUE)

  # Final check to ensure all files are non-empty and expected count matches
  empty_files <- downloaded_files[file.info(downloaded_files)$size == 0]
  if (length(empty_files) > 0) {
    stop(
      "Some files are empty: ",
      paste(basename(empty_files), collapse = ", ")
    )
  }

  if (length(downloaded_files) != length(scheme_info$loci)) {
    stop(
      "Mismatch in the number of downloaded files. Expected: ",
      length(scheme_info$loci),
      " but got: ",
      length(downloaded_files)
    )
  }

  logr::log_print("All files downloaded successfully and are non-empty.")
}


multi_download <- function(
  file_remote,
  file_local,
  total_con = 1000L,
  host_con = 1000L,
  print = TRUE
) {
  # check for duplication
  dups <- duplicated(file_remote) | duplicated(file_local)
  file_remote <- file_remote[!dups]
  file_local <- file_local[!dups]

  # create pool
  pool <- curl::new_pool(total_con = total_con, host_con = host_con)

  # function performed on successful request
  save_download <- function(req) {
    writeBin(req$content, file_local[file_remote == req$url])
  }

  # setup async calls
  invisible(
    lapply(
      file_remote,
      function(f) {
        curl::curl_fetch_multi(f, done = save_download, pool = pool)
      }
    )
  )

  # all created requests are performed here
  out <- curl::multi_run(pool = pool)

  if (print) print(out)
}

download.alleles.CM <- function(url_link, database, folder_name, progress) {
  # Make scheme directory
  directory <- file.path(database, folder_name)
  if (!dir.exists(directory)) {
    tryCatch(
      {
        dir.create(directory, recursive = TRUE)
        message("Directory created: ", directory)
      },
      error = function(e) {
        stop("Failed to create directory: ", directory, "\nError: ", e$message)
      }
    )
  }

  # Make output folder
  output_folder <- file.path(
    database,
    ".downloaded_schemes",
    paste0(folder_name)
  )

  if (!dir.exists(output_folder)) {
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop(
          "Failed to create directory: ",
          output_folder,
          "\nError: ",
          e$message
        )
      }
    )
  } else {
    unlink(output_folder, recursive = TRUE, force = TRUE)
    tryCatch(
      {
        dir.create(output_folder, recursive = TRUE)
        message("Directory created: ", output_folder)
      },
      error = function(e) {
        stop(
          "Failed to create directory: ",
          output_folder,
          "\nError: ",
          e$message
        )
      }
    )
  }

  loci_path <- file.path(database, folder_name, "targets.csv")

  tryCatch(
    {
      download.file(
        paste0(url_link, "/locus/?content-type=csv"),
        dest = loci_path,
        mode = "wb"
      )
      message("Loci info downloaded")
    },
    error = function(e) {
      stop("Failed to download loci info: ", "\nError: ", e$message)
    }
  )

  if (file.exists(loci_path)) {
    loci <- data.table::fread(
      loci_path,
      select = 2,
      sep = "\t",
      header = FALSE
    )[[1]]
    successes <- 0
    count <- length(loci)
    file_local <- file.path(output_folder, paste0(loci, ".fasta"))
    file_remote <- paste0(url_link, "/locus/", loci, ".fasta/")

    while (successes < count) {
      res <- multi_download(
        na.omit(file_remote[1:50]),
        na.omit(file_local[1:50]),
        total_con = 50L,
        host_con = 50L
      )
      remain <- !file.exists(file_local)
      file_local <- file_local[remain]
      file_remote <- file_remote[remain]
      successes <- successes + res$success
      # Increment the progress bar
      progress$inc(res$success / (2 * count))
    }

    progress$set(message = "Compressing Files")

    # Zip folder
    system(paste0(
      "zip -r -j ",
      shQuote(output_folder),
      ".zip ",
      shQuote(output_folder),
      "/"
    ))
    unlink(output_folder, recursive = TRUE)

    # Final check to ensure all files are non-empty and expected count matches
    empty_files <- file_local[file.info(file_local)$size == 0]
    if (length(empty_files) > 0) {
      stop(
        "Some files are empty: ",
        paste(basename(empty_files), collapse = ", ")
      )
    }

    if (length(empty_files) > 0) {
      stop(
        "Mismatch in the number of downloaded files. Expected: ",
        length(file_local),
        " but got: ",
        length(file_local) - length(empty_files)
      )
    }

    logr::log_print("All files downloaded successfully and are non-empty.")
  }
}

# Function to check and remove duplicate segments
process_string <- function(input_string) {
  # Split the string by comma
  segments <- strsplit(input_string, ",")[[1]]

  # Ensure that there are two parts to compare
  if (length(segments) == 2) {
    # Remove asterisks from both segments
    clean_segment_1 <- gsub("\\*", "", segments[1])
    clean_segment_2 <- gsub("\\*", "", segments[2])

    # Check if the cleaned segments are identical
    if (clean_segment_1 == clean_segment_2) {
      # If they are identical, return the cleaned version without asterisk
      return(clean_segment_1)
    } else {
      # If they are not identical, return the original string
      return(input_string)
    }
  } else {
    # If there's no comma or improper format, return the original string
    return(input_string)
  }
}

summarize.AMR <- function(database, scheme) {
  isolates_path <- file.path(database, gsub(" ", "_", scheme), "Isolates")
  isolates_full <- list.files(isolates_path, full.names = TRUE)
  available <- sapply(isolates_full, function(isolate) {
    file.exists(file.path(
      isolate,
      "amrfinder.out"
    ))
  })
  isolates <- basename(isolates_full[available])

  loci <- character()
  amr_profile <- list()
  amr_matches <- data.frame()
  virulence_matches <- data.frame()

  amr_df <- list()
  vir_df <- list()

  for (i in seq_along(isolates)) {
    amr_profile_path <- file.path(isolates_path, isolates[i])

    # amrfinder results
    if (file.exists(file.path(amr_profile_path, "amrfinder.out"))) {
      amr_results <- read.delim(
        file.path(amr_profile_path, "amrfinder.out"),
        stringsAsFactors = FALSE
      )
      gene_symbols <- unique(amr_results$Gene.symbol)
      amr_profile[[i]] <- gene_symbols
      loci <- union(loci, gene_symbols)
    }

    # classification antibiotics drug
    if (file.exists(file.path(amr_profile_path, "summary_matches.txt"))) {
      summary_matches <- read.delim(
        file.path(
          amr_profile_path,
          "summary_matches.txt"
        ),
        stringsAsFactors = FALSE
      ) %>%
        select(-1)

      # Process each column
      for (col in colnames(summary_matches)) {
        if (nrow(summary_matches) > 0) {
          # Ensure the column exists in previous data frames
          if (length(amr_df) > 0 && col %in% colnames(amr_df[[1]])) {
            # Extract previous data frame
            prev_df <- amr_df[[length(amr_df)]]

            if (col %in% colnames(prev_df)) {
              # Combine current and previous columns for the specific column
              combined_col <- unique(
                c(
                  stringr::str_split(prev_df[[col]], ",")[[1]],
                  stringr::str_split(summary_matches[[col]], ",")[[1]]
                )
              )

              # Update column in previous data frame with combined values
              prev_df[[col]] <- paste(combined_col, collapse = ",")
            }

            # Update the last data frame in the list
            amr_df[[length(amr_df)]] <- prev_df
          } else {
            # If thecolumn is new add it to the previous data frame if it exists
            if (length(amr_df) > 0) {
              amr_df[[length(amr_df)]][[col]] <- summary_matches[[col]]
            } else {
              # If there are no previous data frames initialize the first one
              amr_df[[1]] <- summary_matches
            }
          }
        }
      }

      # Add the current data frame to the list
      amr_df[[length(amr_df) + 1]] <- summary_matches
    }

    # classification virulence
    if (file.exists(file.path(amr_profile_path, "summary_virulence.txt"))) {
      summary_virulence <- read.delim(
        file.path(amr_profile_path, "summary_virulence.txt"),
        stringsAsFactors = FALSE
      ) %>%
        select(-1)

      # Process each column
      for (col in colnames(summary_virulence)) {
        if (nrow(summary_virulence) > 0) {
          # Ensure the column exists in previous data frames
          if (length(vir_df) > 0 && col %in% colnames(vir_df[[1]])) {
            # Extract previous data frame
            prev_df <- vir_df[[length(vir_df)]]

            if (col %in% colnames(prev_df)) {
              # Combine the current and previous columns for the specific column
              combined_col <- unique(
                c(
                  stringr::str_split(prev_df[[col]], ",")[[1]],
                  stringr::str_split(summary_virulence[[col]], ",")[[1]]
                )
              )

              # Update the column in previous data frame with combined values
              prev_df[[col]] <- paste(combined_col, collapse = ",")
            }

            # Update the last data frame in the list
            vir_df[[length(vir_df)]] <- prev_df
          } else {
            # If column is new add it to the previous data frame if it exists
            if (length(vir_df) > 0) {
              vir_df[[length(vir_df)]][[col]] <- summary_virulence[[col]]
            } else {
              # If there are no previous data frames, initialize the first one
              vir_df[[1]] <- summary_virulence
            }
          }
        }
      }

      # Add the current data frame to the list
      vir_df[[length(vir_df) + 1]] <- summary_virulence
    }
  }

  # collate amr data frames
  final_df_amr <- dplyr::bind_rows(amr_df, .id = "source")

  if (ncol(final_df_amr) > 1) {
    amr_class_conc <- final_df_amr %>%
      summarise(across(
        everything(),
        ~ {
          unique_values <- unique(unlist(stringr::str_split(na.omit(.), ",")))
          paste(unique_values, collapse = ",")
        }
      )) %>%
      select(-1)

    amr_class_long <- amr_class_conc %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "Variable",
        values_to = "Observation"
      )

    amr_class_pre <- amr_class_long %>%
      tidyr::separate_rows(Observation, sep = ",")

    amr_class <- as.data.frame(lapply(amr_class_pre, function(column) {
      sapply(column, process_string)
    }))

    amr_class <- as.data.frame(lapply(amr_class_pre, function(column) {
      sapply(column, process_string)
    }))
  } else {
    amr_class <- data.frame()
  }

  # collate virulence data frames
  final_df_vir <- dplyr::bind_rows(vir_df, .id = "source")

  if (ncol(final_df_vir) > 1) {
    vir_class_conc <- final_df_vir %>%
      summarise(across(
        everything(),
        ~ {
          unique_values <- unique(unlist(stringr::str_split(na.omit(.), ",")))
          paste(unique_values, collapse = ",")
        }
      )) %>%
      select(-1)

    vir_class_long <- vir_class_conc %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "Variable",
        values_to = "Observation"
      )

    vir_class <- vir_class_long %>%
      tidyr::separate_rows(Observation, sep = ",")
  } else {
    vir_class <- data.frame()
  }

  # collate results
  df <- matrix(FALSE, nrow = length(isolates), ncol = length(loci))
  colnames(df) <- loci
  rownames(df) <- isolates

  for (i in seq_along(amr_profile)) {
    df[i, loci %in% amr_profile[[i]]] <- TRUE
  }

  return(list(
    results = as.data.frame(df),
    AMR_classification = amr_class,
    virulence_classification = vir_class
  ))
}

# function to categorize genes by classes
categorize.gs <- function(names, df) {
  value_amr <- df$amr[which(names == rownames(test))]
  value_vir <- df$vir[which(names == rownames(test))]

  if (!is.na(value_amr) && value_amr != "") {
    return("AMR")
  } else if (!is.na(value_vir) && value_vir != "") {
    return("Vir")
  } else {
    return("None")
  }
}

# Create a function to get color and symbol based on the category
get_annotation_params <- function(
  category_index,
  num_categories,
  color_palette
) {
  # Define color palettes and symbols
  symbol_set <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25) # First symbols set

  # Determine the set based on the category index
  set_index <- (category_index - 1) %/% 10 + 1

  # Calculate the index within the set
  symbol_index <- (category_index - 1) %% 10 + 1
  color_index <- (category_index - 1) %% length(color_palette) + 1

  # Return the symbol and color
  list(symbol = symbol_set[symbol_index], color = color_palette[color_index])
}

# function to get gene screening classification meta
get.gsMeta <- function(gene_class, hm_matrix) {
  class_filtered <- gene_class[
    !duplicated(gsub("\\*", "", gene_class$Observation)),
  ]
  clean_gene_name <- gsub("\\*", "", class_filtered$Observation)
  unison <- colnames(hm_matrix) %in% clean_gene_name

  class_present <- colnames(hm_matrix)[unison]
  no_class <- colnames(hm_matrix)[!unison]

  classes <- character()
  for (i in 1:length(class_present)) {
    classes[i] <- class_filtered$Variable[which(
      class_present[i] == clean_gene_name
    )]
  }

  meta <- data.frame(
    gene = c(class_present, no_class),
    class = c(classes, rep(NA, length(no_class)))
  )

  if (nrow(meta) != 0) {
    meta <- meta %>%
      arrange(gene) %>%
      tibble::column_to_rownames(var = "gene")
  }

  return(meta)
}

color_scale_bg_JS <- "var selectedOption = $('#col_scale_id').val();
    
    if (selectedOption === 'Set1') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #E41A1C 0%, #E41A1C 11%, 
        #377EB8 11%, #377EB8 22%, #4DAF4A 22%, #4DAF4A 33%, #984EA3 33%, 
        #984EA3 44%, #FF7F00 44%, #FF7F00 55%, #FFFF33 55%, #FFFF33 66%, 
        #A65628 66%, #A65628 77%, #F781BF 77%, #F781BF 88%, #999999 88%, 
        #999999 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Set2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #66C2A5 0%, #66C2A5 12.5%, 
        #FC8D62 12.5%, #FC8D62 25%, #8DA0CB 25%, #8DA0CB 37.5%, #E78AC3 37.5%, 
        #E78AC3 50%, #A6D854 50%, #A6D854 62.5%, #FFD92F 62.5%, #FFD92F 75%, 
        #E5C494 75%, #E5C494 87.5%, #B3B3B3 87.5%, #B3B3B3 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Set3') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #8DD3C7 0%, #8DD3C7 8.33333%, 
        #FFFFB3 8.33333%, #FFFFB3 16.6667%, #BEBADA 16.6667%, #BEBADA 25%, 
        #FB8072 25%, #FB8072 33.3333%, #80B1D3 33.3333%, #80B1D3 41.6667%, 
        #FDB462 41.6667%, #FDB462 50%, #B3DE69 50%, #B3DE69 58.3333%, 
        #FCCDE5 58.3333%, #FCCDE5 66.6667%, #D9D9D9 66.6667%, #D9D9D9 75%, 
        #BC80BD 75%, #BC80BD 83.3333%, #CCEBC5 83.3333%, #CCEBC5 91.6667%, 
        #FFED6F 91.6667%, #FFED6F 100%)',
        'color': 'black'
      });
    } else if (selectedOption === 'Pastel1') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FBB4AE 0%, #FBB4AE 11.1111%, 
        #B3CDE3 11.1111%, #B3CDE3 22.2222%, #CCEBC5 22.2222%, #CCEBC5 33.3333%, 
        #DECBE4 33.3333%, #DECBE4 44.4444%, #FED9A6 44.4444%, #FED9A6 55.5556%, 
        #FFFFCC 55.5556%, #FFFFCC 66.6667%, #E5D8BD 66.6667%, #E5D8BD 77.7778%, 
        #FDDAEC 77.7778%, #FDDAEC 88.8889%, #F2F2F2 88.8889%, #F2F2F2 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Pastel2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #B3E2CD 0%, #B3E2CD 12.5%, 
        #FDCDAC 12.5%, #FDCDAC 25%, #CBD5E8 25%, #CBD5E8 37.5%, #F4CAE4 37.5%, 
        #F4CAE4 50%, #E6F5C9 50%, #E6F5C9 62.5%, #FFF2AE 62.5%, #FFF2AE 75%, 
        #F1E2CC 75%, #F1E2CC 87.5%, #CCCCCC 87.5%, #CCCCCC 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Paired') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #A6CEE3 0%, #A6CEE3 8.33333%, 
        #1F78B4 8.33333%, #1F78B4 16.6667%, #B2DF8A 16.6667%, #B2DF8A 25%, 
        #33A02C 25%, #33A02C 33.3333%, #FB9A99 33.3333%, #FB9A99 41.6667%, 
        #E31A1C 41.6667%, #E31A1C 50%, #FDBF6F 50%, #FDBF6F 58.3333%, 
        #FF7F00 58.3333%, #FF7F00 66.6667%, #CAB2D6 66.6667%, #CAB2D6 75%, 
        #6A3D9A 75%, #6A3D9A 83.3333%, #FFFF99 83.3333%, #FFFF99 91.6667%, 
        #B15928 91.6667%, #B15928 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Dark2') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #1B9E77 0%, #1B9E77 12.5%, 
        #D95F02 12.5%, #D95F02 25%, #7570B3 25%, #7570B3 37.5%, #E7298A 37.5%, 
        #E7298A 50%, #66A61E 50%, #66A61E 62.5%, #E6AB02 62.5%, #E6AB02 75%, 
        #A6761D 75%, #A6761D 87.5%, #666666 87.5%, #666666 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'Accent') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #7FC97F 0%, #7FC97F 12.5%, 
        #BEAED4 12.5%, #BEAED4 25%, #FDC086 25%, #FDC086 37.5%, #FFFF99 37.5%, 
        #FFFF99 50%, #386CB0 50%, #386CB0 62.5%, #F0027F 62.5%, #F0027F 75%, 
        #BF5B17 75%, #BF5B17 87.5%, #666666 87.5%, #666666 100%)',
      'color': 'black'
      })
    } else if (selectedOption === 'YlOrRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFCC 0%, #FFFFCC 11.1111%, 
        #FFEDA0 11.1111%, #FFEDA0 22.2222%, #FED976 22.2222%, #FED976 33.3333%, 
        #FEB24C 33.3333%, #FEB24C 44.4444%, #FD8D3C 44.4444%, #FD8D3C 55.5556%, 
        #FC4E2A 55.5556%, #FC4E2A 66.6667%, #E31A1C 66.6667%, #E31A1C 77.7778%, 
        #BD0026 77.7778%, #BD0026 88.8889%, #800026 88.8889%, #800026 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlOrBr') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFE5 0%, #FFFFE5 11.1111%, 
        #FFF7BC 11.1111%, #FFF7BC 22.2222%, #FEE391 22.2222%, #FEE391 33.3333%, 
        #FEC44F 33.3333%, #FEC44F 44.4444%, #FE9929 44.4444%, #FE9929 55.5556%, 
        #EC7014 55.5556%, #EC7014 66.6667%, #CC4C02 66.6667%, #CC4C02 77.7778%, 
        #993404 77.7778%, #993404 88.8889%, #662506 88.8889%, #662506 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlGnBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFD9 0%, #FFFFD9 11.1111%, 
        #EDF8B1 11.1111%, #EDF8B1 22.2222%, #C7E9B4 22.2222%, #C7E9B4 33.3333%, 
        #7FCDBB 33.3333%, #7FCDBB 44.4444%, #41B6C4 44.4444%, #41B6C4 55.5556%, 
        #1D91C0 55.5556%, #1D91C0 66.6667%, #225EA8 66.6667%, #225EA8 77.7778%, 
        #253494 77.7778%, #253494 88.8889%, #081D58 88.8889%, #081D58 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'YlGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFE5 0%, #FFFFE5 11.1111%, 
        #F7FCB9 11.1111%, #F7FCB9 22.2222%, #D9F0A3 22.2222%, #D9F0A3 33.3333%, 
        #ADDD8E 33.3333%, #ADDD8E 44.4444%, #78C679 44.4444%, #78C679 55.5556%,
        #41AB5D 55.5556%, #41AB5D 66.6667%, #238443 66.6667%, #238443 77.7778%, 
        #006837 77.7778%, #006837 88.8889%, #004529 88.8889%, #004529 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Reds') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF5F0 0%, #FFF5F0 11.1111%, 
        #FEE0D2 11.1111%, #FEE0D2 22.2222%, #FCBBA1 22.2222%, #FCBBA1 33.3333%, 
        #FC9272 33.3333%, #FC9272 44.4444%, #FB6A4A 44.4444%, #FB6A4A 55.5556%, 
        #EF3B2C 55.5556%, #EF3B2C 66.6667%, #CB181D 66.6667%, #CB181D 77.7778%, 
        #A50F15 77.7778%, #A50F15 88.8889%, #67000D 88.8889%, #67000D 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'RdPu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7F3 0%, #FFF7F3 11.1111%, 
        #FDE0DD 11.1111%, #FDE0DD 22.2222%, #FCC5C0 22.2222%, #FCC5C0 33.3333%, 
        #FA9FB5 33.3333%, #FA9FB5 44.4444%, #F768A1 44.4444%, #F768A1 55.5556%, 
        #DD3497 55.5556%, #DD3497 66.6667%, #AE017E 66.6667%, #AE017E 77.7778%, 
        #7A0177 77.7778%, #7A0177 88.8889%, #49006A 88.8889%, #49006A 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Purples') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FCFBFD 0%, #FCFBFD 11.1111%, 
        #EFEDF5 11.1111%, #EFEDF5 22.2222%, #DADAEB 22.2222%, #DADAEB 33.3333%, 
        #BCBDDC 33.3333%, #BCBDDC 44.4444%, #9E9AC8 44.4444%, #9E9AC8 55.5556%, 
        #807DBA 55.5556%, #807DBA 66.6667%, #6A51A3 66.6667%, #6A51A3 77.7778%, 
        #54278F 77.7778%, #54278F 88.8889%, #3F007D 88.8889%, #3F007D 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7F4F9 0%, #F7F4F9 11.1111%, 
        #E7E1EF 11.1111%, #E7E1EF 22.2222%, #D4B9DA 22.2222%, #D4B9DA 33.3333%, 
        #C994C7 33.3333%, #C994C7 44.4444%, #DF65B0 44.4444%, #DF65B0 55.5556%,
        #E7298A 55.5556%, #E7298A 66.6667%, #CE1256 66.6667%, #CE1256 77.7778%, 
        #980043 77.7778%, #980043 88.8889%, #67001F 88.8889%, #67001F 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuBuGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7FB 0%, #FFF7FB 11.1111%, 
        #ECE2F0 11.1111%, #ECE2F0 22.2222%, #D0D1E6 22.2222%, #D0D1E6 33.3333%, 
        #A6BDDB 33.3333%, #A6BDDB 44.4444%, #67A9CF 44.4444%, #67A9CF 55.5556%, 
        #3690C0 55.5556%, #3690C0 66.6667%, #02818A 66.6667%, #02818A 77.7778%, 
        #016C59 77.7778%, #016C59 88.8889%, #014636 88.8889%, #014636 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'PuBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7FB 0%, #FFF7FB 11.1111%, 
        #ECE7F2 11.1111%, #ECE7F2 22.2222%, #D0D1E6 22.2222%, #D0D1E6 33.3333%,
        #A6BDDB 33.3333%, #A6BDDB 44.4444%, #74A9CF 44.4444%, #74A9CF 55.5556%, 
        #3690C0 55.5556%, #3690C0 66.6667%, #0570B0 66.6667%, #0570B0 77.7778%, 
        #045A8D 77.7778%, #045A8D 88.8889%, #023858 88.8889%, #023858 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'OrRd') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF7EC 0%, #FFF7EC 11.1111%, 
        #FEE8C8 11.1111%, #FEE8C8 22.2222%, #FDD49E 22.2222%, #FDD49E 33.3333%, 
        #FDBB84 33.3333%, #FDBB84 44.4444%, #FC8D59 44.4444%, #FC8D59 55.5556%, 
        #EF6548 55.5556%, #EF6548 66.6667%, #D7301F 66.6667%, #D7301F 77.7778%, 
        #B30000 77.7778%, #B30000 88.8889%, #7F0000 88.8889%, #7F0000 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Oranges') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFF5EB 0%, #FFF5EB 11.1111%, 
        #FEE6CE 11.1111%, #FEE6CE 22.2222%, #FDD0A2 22.2222%, #FDD0A2 33.3333%, 
        #FDAE6B 33.3333%, #FDAE6B 44.4444%, #FD8D3C 44.4444%, #FD8D3C 55.5556%, 
        #F16913 55.5556%, #F16913 66.6667%, #D94801 66.6667%, #D94801 77.7778%, 
        #A63603 77.7778%, #A63603 88.8889%, #7F2704 88.8889%, #7F2704 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Greys') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #FFFFFF 0%, #FFFFFF 11.1111%, 
        #F0F0F0 11.1111%, #F0F0F0 22.2222%, #D9D9D9 22.2222%, #D9D9D9 33.3333%, 
        #BDBDBD 33.3333%, #BDBDBD 44.4444%, #969696 44.4444%, #969696 55.5556%, 
        #737373 55.5556%, #737373 66.6667%, #525252 66.6667%, #525252 77.7778%, 
        #252525 77.7778%, #252525 88.8889%, #000000 88.8889%, #000000 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Greens') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCF5 0%, #F7FCF5 11.1111%, 
        #E5F5E0 11.1111%, #E5F5E0 22.2222%, #C7E9C0 22.2222%, #C7E9C0 33.3333%,
        #A1D99B 33.3333%, #A1D99B 44.4444%, #74C476 44.4444%, #74C476 55.5556%,
        #41AB5D 55.5556%, #41AB5D 66.6667%, #238B45 66.6667%, #238B45 77.7778%, 
        #006D2C 77.7778%, #006D2C 88.8889%, #00441B 88.8889%, #00441B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'GnBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCF0 0%, #F7FCF0 11.1111%, 
        #E0F3DB 11.1111%, #E0F3DB 22.2222%, #CCEBC5 22.2222%, #CCEBC5 33.3333%,
        #A8DDB5 33.3333%, #A8DDB5 44.4444%, #7BCCC4 44.4444%, #7BCCC4 55.5556%, 
        #4EB3D3 55.5556%, #4EB3D3 66.6667%, #2B8CBE 66.6667%, #2B8CBE 77.7778%, 
        #0868AC 77.7778%, #0868AC 88.8889%, #084081 88.8889%, #084081 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'BuPu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCFD 0%, #F7FCFD 11.1111%, 
        #E0ECF4 11.1111%, #E0ECF4 22.2222%, #BFD3E6 22.2222%, #BFD3E6 33.3333%,
        #9EBCDA 33.3333%, #9EBCDA 44.4444%, #8C96C6 44.4444%, #8C96C6 55.5556%, 
        #8C6BB1 55.5556%, #8C6BB1 66.6667%, #88419D 66.6667%, #88419D 77.7778%, 
        #810F7C 77.7778%, #810F7C 88.8889%, #4D004B 88.8889%, #4D004B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'BuGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FCFD 0%, #F7FCFD 11.1111%, 
        #E5F5F9 11.1111%, #E5F5F9 22.2222%, #CCECE6 22.2222%, #CCECE6 33.3333%, 
        #99D8C9 33.3333%, #99D8C9 44.4444%, #66C2A4 44.4444%, #66C2A4 55.5556%, 
        #41AE76 55.5556%, #41AE76 66.6667%, #238B45 66.6667%, #238B45 77.7778%, 
        #006D2C 77.7778%, #006D2C 88.8889%, #00441B 88.8889%, #00441B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'Blues') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #F7FBFF 0%, #F7FBFF 11.1111%, 
        #DEEBF7 11.1111%, #DEEBF7 22.2222%, #C6DBEF 22.2222%, #C6DBEF 33.3333%, 
        #9ECAE1 33.3333%, #9ECAE1 44.4444%, #6BAED6 44.4444%, #6BAED6 55.5556%, 
        #4292C6 55.5556%, #4292C6 66.6667%, #2171B5 66.6667%, #2171B5 77.7778%,
        #08519C 77.7778%, #08519C 88.8889%, #08306B 88.8889%, #08306B 100%)',
      'color': 'white'
      })
    } else if (selectedOption === 'magma') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #000004FF, #07071DFF, 
        #160F3BFF, #29115AFF, #400F73FF, #56147DFF, #6B1D81FF, #802582FF, 
        #952C80FF, #AB337CFF, #C03A76FF, #D6456CFF, #E85362FF, #F4685CFF, 
        #FA815FFF, #FD9A6AFF, #FEB37BFF, #FECC8FFF, #FDE4A6FF, #FCFDBFFF)',
      'color': 'white'
      })
    } else if (selectedOption === 'inferno') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #000004FF, #08051EFF, 
        #190C3EFF, #300A5BFF, #460B6AFF, #5C126EFF, #711A6EFF, #87216BFF, 
        #9C2964FF, #B1325AFF, #C43C4EFF, #D64B40FF, #E55C30FF, #F17020FF, 
        #F8870EFF, #FCA007FF, #FBB91FFF, #F7D340FF, #F1ED6FFF, #FCFFA4FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'plasma') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #0D0887FF, #2D0594FF, 
        #44039EFF, #5901A5FF, #6F00A8FF, #8305A7FF, #9512A1FF, #A72197FF, 
        #B6308BFF, #C5407EFF, #D14E72FF, #DD5E66FF, #E76E5BFF, #EF7F4FFF, 
        #F79044FF, #FBA238FF, #FEB72DFF, #FDCB26FF, #F7E225FF, #F0F921FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'viridis') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #440154FF, #481568FF,
        #482677FF, #453781FF, #3F4788FF, #39558CFF, #32648EFF, #2D718EFF,
        #287D8EFF, #238A8DFF, #1F968BFF, #20A386FF, #29AF7FFF, #3CBC75FF, 
        #56C667FF, #74D055FF, #94D840FF, #B8DE29FF, #DCE318FF, #FDE725FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'cividis') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #00204DFF, #002A64FF, 
        #00336FFF, #1F3C6DFF, #35466BFF, #444F6BFF, #53596CFF, #5F636EFF, 
        #6B6C71FF, #777776FF, #838079FF, #908B79FF, #9D9677FF, #ABA074FF, 
        #B9AC70FF, #C7B76BFF, #D7C463FF, #E5D05AFF, #F5DD4DFF, #FFEA46FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'rocket') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #03051AFF, #150E26FF, 
        #281535FF, #3C1A42FF, #511E4DFF, #661F54FF, #7C1F5AFF, #931C5BFF, 
        #AA185AFF, #C11754FF, #D3214BFF, #E33541FF, #ED4E3EFF, #F26847FF, 
        #F4815AFF, #F5986FFF, #F6AE86FF, #F7C2A2FF, #F8D7BFFF, #FAEBDDFF)',
      'color': 'white'
      })
    } else if (selectedOption === 'mako') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #0B0405FF, #190E19FF, 
        #27182DFF, #312142FF, #3A2C59FF, #3F3770FF, #414388FF, #3C5397FF, 
        #38639DFF, #3573A1FF, #3482A4FF, #3491A8FF, #35A0ABFF, #3AAEADFF, 
        #46BEADFF, #5ACCADFF, #7ED7AFFF, #A4E0BBFF, #C3E9CEFF, #DEF5E5FF)',
      'color': 'white'
      })
    } else if (selectedOption === 'turbo') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #30123BFF, #3F3994FF, 
        #455ED2FF, #4681F7FF, #3AA2FCFF, #23C3E4FF, #18DEC1FF, #2CF09EFF, 
        #5BFB72FF, #8EFF49FF, #B5F836FF, #D6E635FF, #EFCD3AFF, #FCB036FF, 
        #FD8A26FF, #F36215FF, #E14209FF, #C82803FF, #A51301FF, #7A0403FF)',
      'color': 'black'
      })
    } else if (selectedOption === 'Spectral') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #9E0142, #D53E4F, #F46D43, 
        #FDAE61, #FEE08B, #FFFFBF, #E6F598, #ABDDA4, #66C2A5, #3288BD, 
        #5E4FA2)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdYlGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #A50026, #D73027, #F46D43, 
        #FDAE61, #FEE08B, #FFFFBF, #D9EF8B, #A6D96A, #66BD63, #1A9850, 
        #006837)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdGy') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #67001F, #B2182B, #D6604D, 
        #F4A582, #FDDBC7, #FFFFFF, #E0E0E0, #BABABA, #878787, #4D4D4D, 
        #1A1A1A)',
      'color': 'black'
      })
    } else if (selectedOption === 'RdBu') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #67001F, #B2182B, #D6604D, 
        #F4A582, #FDDBC7, #F7F7F7, #D1E5F0, #92C5DE, #4393C3, #2166AC, 
        #053061)',
      'color': 'black'
      })
    } else if (selectedOption === 'PuOr') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #7F3B08, #B35806, #E08214,
        #FDB863, #FEE0B6, #F7F7F7, #D8DAEB, #B2ABD2, #8073AC, #542788, 
        #2D004B)',
      'color': 'black'
      })
    } else if (selectedOption === 'PRGn') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #40004B, #762A83, #9970AB, 
        #C2A5CF, #E7D4E8, #F7F7F7, #D9F0D3, #A6DBA0, #5AAE61, #1B7837,
        #00441B)',
      'color': 'black'
      })
    } else if (selectedOption === 'PiYG') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #8E0152, #C51B7D, #DE77AE, 
        #F1B6DA, #FDE0EF, #F7F7F7, #E6F5D0, #B8E186, #7FBC41, #4D9221, 
        #276419)',
      'color': 'black'
      })
    } else if (selectedOption === 'BrBG') {
      $('#col_scale_id_ui .selectize-input').css({
        'background': 'linear-gradient(to right, #543005, #8C510A, #BF812D, 
        #DFC27D, #F6E8C3, #F5F5F5, #C7EAE5, #80CDC1, #35978F, #01665E, 
        #003C30)',
      'color': 'black'
      })
    }"
