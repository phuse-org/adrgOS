#' List ADaM Programs, Packages, and Functions
#'
#' This function reads R ADaM programs to list all programs used and the packages/functions
#' within each. It offers two output formats controlled by the \code{adrg} flag: a nested
#' list (for data access) or a flattened data frame (for documentation).
#' It automatically derives the expected ADaM dataset name (\code{.xpt}) from the program file name.
#'
#' @param target_dir This is the file path to the program(s) that are to be processed.
#'   If \code{all_one_file} is 'YN', this should be a **directory path**. If
#'   \code{all_one_file} is 'NY', this should be the **full path to a single R file**.
#'   Defaults to the current working directory (\code{.}).
#' @param all_one_file This flag controls processing scope:
#'   \itemize{
#'     \item \code{'YN'} (Default): Process **All** ADaM files in \code{target_dir} (matching \code{^ad.*\\.[rR]$}).
#'     \item \code{'NY'}: Process **Only** the single file specified by \code{target_dir} (matching \code{^ad.*\\.[rR]$})..
#'   }
#' @param adrg Flag to select the output format:
#'   \itemize{
#'     \item \code{'Y'} (Default): Returns a **Data Frame** formatted for an Analysis Data Reviewers Guide (ADRG), with all package/function usage consolidated into a single string per program.
#'     \item \code{'N'}: Returns a **Nested List** structure, where each program's package/function details are stored in a nested data frame for easy programmatic access.
#'   }
#' @return The output format depends on the \code{adrg} flag:
#'   \itemize{
#'     \item **If \code{adrg = 'Y'} (Data Frame):** Columns are \code{program}, \code{dataset}, and \code{functions} (a single, newline-separated string).
#'     \item **If \code{adrg = 'N'} (Nested List):** A list where each element has:
#'        \itemize{
#'          \item \code{program}: The R program file name.
#'          \item \code{dataset}: The derived ADaM dataset name.
#'          \item \code{packages_used}: A nested data frame with columns \code{package} and \code{func_names}.
#'        }
#'   }
#' @importFrom NCmisc list.functions.in.file
#' @importFrom dplyr select mutate bind_rows group_by summarise ungroup
#' @importFrom stringr str_detect
#' @importFrom tidyr nest
#' @importFrom purrr pmap
#' @export
#'
#' @examples
#' \dontrun{
#' # NOTE: These examples use non-portable file paths and require the files to exist.
#'
#' # --- Scenario 1: Process ALL files, Output for ADRG (adrg='Y', default) ---
#' # Returns a single data frame with collapsed functions per program.
#' adrg_all_adams_df <- list_adam_pgms(
#'   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/",
#'   all_one_file = 'YN',
#'   adrg = 'Y'
#' )
#' print(adrg_all_adams_df)
#'
#' # --- Scenario 2: Process ALL files, Output as Nested List (adrg='N') ---
#' # Returns a list where each program's details are nested. Ideal for programmatic access.
#' all_adams_list <- list_adam_pgms(
#'   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/",
#'   all_one_file = 'YN',
#'   adrg = 'N'
#' )
#' # Accessing the nested packages_used data frame for the first program:
#' all_adams_list[[1]]$packages_used
#'
#' # --- Scenario 3: Process ONE file, Output for ADRG (adrg='Y', default) ---
#' # Returns a data frame with one row, formatted for the ADRG.
#' adrg_one_adam_df <- list_adam_pgms(
#'    target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/adlbc.r",
#'    all_one_file = 'NY',
#'    adrg = 'Y'
#' )
#' print(adrg_one_adam_df)
#'
#' # --- Scenario 4: Process ONE file, Output as Nested List (adrg='N') ---
#' # Returns a list with one element (the specified program's details).
#' one_adam_list <- list_adam_pgms(
#'   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/adlbc.r",
#'   all_one_file = 'NY',
#'   adrg = 'N'
#' )
#' print(one_adam_list)
#' }
list_adam_pgms <- function(target_dir = ".", all_one_file = 'YN', adrg = 'Y') {

  # ======================================================================
  # 0. Input and Argument Validation
  # ======================================================================

  # Validate all_one_file flag
  if (!(all_one_file %in% c('YN', 'NY'))) {
    stop("Error: 'all_one_file' must be either 'YN' or 'NY'.")
  }
  # Validate adrg flag
  if (!(adrg %in% c('Y', 'N'))) {
    stop("Error: 'adrg' must be either 'Y' or 'N'.")
  }

  if (all_one_file == 'YN') {
    # Check if directory exists for 'YN' mode
    if (!dir.exists(target_dir)) {
      stop(paste0("Error: Directory not found. 'all_one_file = 'YN', please read the manual. Check 'target_dir' path to ensure you are not pointing to a specific file or a non-existent directory: ", target_dir))
    }
    # Get a list of all files matching the ADaM pattern
    r_files <- list.files(
      path = target_dir,
      pattern = "^ad.*\\.[rR]$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(r_files) == 0) {
      warning(paste0("Warning: No ADaM R programs found matching pattern '^ad.*\\.[rR]$' in: ", target_dir))
      # Return empty list or dataframe based on adrg flag
      return(if (adrg == 'Y') data.frame(program = character(0), dataset = character(0), functions = character(0)) else list())
    }
  } else if (all_one_file == 'NY') {
    # Check if single file exists for 'NY' mode
    if (!file.exists(target_dir)) {
      stop(paste0("Error: File not found. Please check the file exists in path: ", target_dir))
    }
    if (dir.exists(target_dir)) {
      stop(paste0("Error: Please read the manual. Expected a single file path for 'all_one_file = 'NY'', but a directory was provided: ", target_dir))
    }
    if (!grepl("^ad.*\\.[rR]$", basename(target_dir), ignore.case = TRUE)) {
      stop(paste0("Error: The file '", basename(target_dir), "' does not match the required ADaM program naming pattern ('^ad.*\\.[rR]$'). ",
                  "Please ensure the file name starts with 'ad' and ends with '.r' or '.R'."
      ))
    }
    r_files <- target_dir
  }

  # Initialize a list to store the results from each file
  all_file_results <- list()

  # ======================================================================
  # 1. Function to process a single file (No change)
  # ======================================================================

  process_r_file <- function(file_path) {
    program_name <- basename(file_path)

    # 1. Read the code (wrapped for robustness)
    code <- tryCatch(
      readLines(file_path, warn = FALSE),
      error = function(e) {
        warning(paste("Warning: Failed to read file", program_name, "due to I/O error. Skipping."))
        return(NULL)
      }
    )
    if (is.null(code)) return(NULL)

    # ... (Existing logic for matching library(), require(), and loading packages)
    lib_pattern <- '(library|require)\\s*\\(\\s*["\']?([^"\'\\)\\s]+)["\']?\\s*\\)'
    matches <- regmatches(code, gregexpr(lib_pattern, code))
    pkgs <- unique(unlist(lapply(matches, function(m) {
      gsub(lib_pattern, '\\2', m)
    })))
    invisible(lapply(pkgs, function(p) {
      suppressPackageStartupMessages(
        try(require(p, character.only = TRUE, quietly = TRUE), silent = TRUE)
      )
    }))

    # 2. Get functions from NCmisc (wrapped for robustness against syntax errors)
    func_list <- tryCatch(
      NCmisc::list.functions.in.file(file_path),
      error = function(e) {
        warning(paste("Warning: Failed to parse file", program_name, ". Check for syntax errors. Skipping."))
        return(NULL)
      }
    )
    if (is.null(func_list)) return(NULL)

    # ... (Existing logic for processing and cleaning func_list into result_df)
    df_list <- list()
    for (name in names(func_list)) {
      funcs <- func_list[[name]]
      if (length(funcs) == 0) next

      if (grepl("^c\\(", name)) {
        pkgs_list <- gsub('"', '', unlist(strsplit(gsub("c\\(|\\)", "", name), ", ")))
        pkg_str <- paste(pkgs_list, collapse = ", ")
      } else {
        pkg_str <- gsub('"', '', name)
      }

      pkg_str <- gsub("character\\(0\\)", "user_defined", pkg_str)
      pkg_str <- gsub("package:", "", pkg_str)

      df_list[[length(df_list) + 1]] <- data.frame(
        func_name = funcs,
        package = pkg_str,
        stringsAsFactors = FALSE
      )
    }

    if (length(df_list) == 0) return(NULL)

    result <- do.call(rbind, df_list)

    result_df <- result %>%
      group_by(package) %>%
      summarise(
        func_names = paste(func_name, collapse = ", ")
      ) %>%
      ungroup()

    result_df$program <- program_name
    result_df <- result_df %>%
      select(program, package, func_names)

    return(result_df)
  }

  # ======================================================================
  # 2. Iterate over all files and combine results (No change)
  # ======================================================================

  for (file in r_files) {
    cat(paste("Processing:", basename(file), "\n"))

    file_result <- process_r_file(file)

    if (!is.null(file_result)) {
      all_file_results[[length(all_file_results) + 1]] <- file_result
    }
  }

  # ======================================================================
  # 3. Final aggregation and formatting
  # ======================================================================

  results_df <- bind_rows(all_file_results)

  # Check if results_df is empty before piping
  if (nrow(results_df) == 0 && length(r_files) > 0) {
    warning("No function usage found in any processed files.")
    return(if (adrg == 'Y') data.frame(program = character(0), dataset = character(0), functions = character(0)) else list())
  } else if (nrow(results_df) == 0) {
    return(if (adrg == 'Y') data.frame(program = character(0), dataset = character(0), functions = character(0)) else list())
  }

  # Derive dataset name (needed for both output types)
  results_df$dataset <- gsub("\\.[rR]$", ".xpt", results_df$program)

  if (adrg == 'N') {
    # Output Type: Nested List (for programmatic access)
    # 1. Group by program/dataset and NEST the package/function details
    nested_data <- results_df %>%
      select(program, dataset, package, func_names) %>%
      group_by(program, dataset) %>%
      tidyr::nest(packages_used = c(package, func_names)) %>%
      ungroup()

    # 2. Convert the nested data frame into a list of lists (one element per program)
    final_list <- purrr::pmap(nested_data, list)

    # 3. return final_list with list of package and functions
    return(final_list)

  } else if (adrg == 'Y') {
    # Output Type: Data Frame (for ADRG documentation)
    # 1. Combine package and functions into a single string
    results_df <- results_df %>% mutate(functions = paste0(package,": ",func_names))

    # 2. Reorder columns
    list_adams <- results_df %>%
      select(program, dataset, functions)

    # 3. Collapse all function strings into one cell per program
    adrg_adams <- list_adams %>%
      group_by(program, dataset) %>%
      summarise(
        functions = paste(functions, collapse = "\n\n"),
        .groups = 'drop'
      )

    # 4. return table needed for the ADRG
    return(adrg_adams)
  }
}
