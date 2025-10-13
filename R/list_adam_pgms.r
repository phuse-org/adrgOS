#' List ADaM Programs, Packages, and Functions
#'
#' This function reads in R ADaM programs used in R Submissions to list out
#' all of the R ADaM programs used and the packages and functions used
#' within each R ADaM program. It automatically derives the expected ADaM
#' dataset name (`.xpt`) from the program file name.
#'
#' @param target_dir This is the file path to the program(s) that are to be processed.
#'   If \code{all_one_file} is 'YN', this should be a **directory path**. If
#'   \code{all_one_file} is 'NY', this should be the **full path to a single R file**.
#'   Defaults to the current working directory (`.`).
#' @param all_one_file This flag allows all programs in a directory to be processed
#'   or only one specific program file.
#'   \itemize{
#'     \item \code{'YN'} (Default): Process **All** ADaM files in \code{target_dir} (matching \code{^ad.*\\.[rR]$}).
#'     \item \code{'NY'}: Process **Only** the single file specified by \code{target_dir}.
#'   }
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item \code{program}: The name of the R program file (e.g., "adsl.r").
#'     \item \code{dataset}: The derived ADaM dataset name (e.g., "adsl.xpt").
#'     \item \code{functions}: A character string listing all packages and
#'           their associated functions used in the program (e.g., "admiral: derive_var_dt, derive_vars_duration").
#'   }
#' @importFrom NCmisc list.functions.in.file
#' @importFrom dplyr select mutate bind_rows group_by summarise ungroup
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming the directory exists with R files starting with 'ad'
#' # 1. Process ALL programs in a directory (all_one_file = 'YN')
#' all_adams <- list_adam_pgms(
#'   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/",
#'   all_one_file = 'YN'
#' )
#'
#' # 2. Process ONLY one specified program (all_one_file = 'NY')
#' one_adam <- list_adam_pgms(
#'   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/adtte.r",
#'   all_one_file = 'NY'
#' )
#' print(one_adam)
#' }
list_adam_pgms <- function(target_dir = ".", all_one_file = 'YN') {
  library(NCmisc)
  library(dplyr)
  library(stringr)

  # ----------------------------------------------------------------------
  # 1. Define the directory and get the list of R files
  # ----------------------------------------------------------------------

  # Replace this with the path to your directory of R programs
  # target_dir <- "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/"

  if (all_one_file == 'YN') {
  # Get a list of all files ending in '.R' or '.r'
    r_files <- list.files(
    path = target_dir,
    # pattern = "\\.r$",
    pattern = "^ad.*\\.[rR]$",
    full.names = TRUE,
    ignore.case = TRUE
    )
  } else if (all_one_file == 'NY') {
  r_files <- target_dir
  }

  # Initialize a list to store the results from each file
  all_file_results <- list()

  # ----------------------------------------------------------------------
  # 2. Function to process a single file (Encapsulating your original logic)
  # ----------------------------------------------------------------------

  process_r_file <- function(file_path) {
    # 1. Get the simple program name (e.g., "adsl.r")
    program_name <- basename(file_path)

    # 2. Read the code
    code <- readLines(file_path, warn = FALSE)

    # 3. Match library() and require()
    # (NOTE: Your original package loading/matching logic is robust and kept here)
    lib_pattern <- '(library|require)\\s*\\(\\s*["\']?([^"\'\\)\\s]+)["\']?\\s*\\)'
    matches <- regmatches(code, gregexpr(lib_pattern, code))

    pkgs <- unique(unlist(lapply(matches, function(m) {
      gsub(lib_pattern, '\\2', m)
    })))

    # 4. Load packages (Necessary for NCmisc::list.functions.in.file to resolve namespaces)
    invisible(lapply(pkgs, function(p) {
      suppressPackageStartupMessages(
        try(require(p, character.only = TRUE, quietly = TRUE), silent = TRUE)
      )
    }))

    # 5. Get functions from NCmisc
    # This returns a list where names are packages and values are function names
    func_list <- NCmisc::list.functions.in.file(file_path)

    # 6. Process into clean data frame (your original cleaning logic)
    df_list <- list()
    for (name in names(func_list)) {
      funcs <- func_list[[name]]
      if (length(funcs) == 0) next

      # Handle multiple packages (e.g., from base functions)
      if (grepl("^c\\(", name)) {
        pkgs_list <- gsub('"', '', unlist(strsplit(gsub("c\\(|\\)", "", name), ", ")))
        pkg_str <- paste(pkgs_list, collapse = ", ")
      } else {
        pkg_str <- gsub('"', '', name)
      }

      # Clean up package names
      pkg_str <- gsub("character\\(0\\)", "user_defined", pkg_str)
      pkg_str <- gsub("package:", "", pkg_str) # Remove "package:" string

      df_list[[length(df_list) + 1]] <- data.frame(
        func_name = funcs,
        package = pkg_str,
        stringsAsFactors = FALSE
      )
    }

    # 7. Combine the list of dataframes for the current file
    if (length(df_list) == 0) return(NULL)

    result <- do.call(rbind, df_list)

    # 8. Transpose/Aggregate the result (your final logic)
    result_df <- result %>%
      group_by(package) %>%
      summarise(
        func_names = paste(func_name, collapse = ", ")
      ) %>%
      ungroup()

    # 9. Add the program name column
    result_df$program <- program_name

    # 10. Reorder columns to match the request: program, package, func_names
    result_df <- result_df %>%
      select(program, package, func_names)

    return(result_df)
  }

  # ----------------------------------------------------------------------
  # 3. Iterate over all files and combine results
  # ----------------------------------------------------------------------

  for (file in r_files) {
    cat(paste("Processing:", basename(file), "\n"))

    file_result <- process_r_file(file)

    if (!is.null(file_result)) {
      all_file_results[[length(all_file_results) + 1]] <- file_result
    }
  }

  # 4. Final aggregation of all results and combine package and functions
  results_df <- bind_rows(all_file_results)
  results_df <- results_df %>% mutate(functions = paste0(package,": ",func_names))


  # 5. create column for datasets generated by program
  results_df$dataset <- gsub("\\.[rR]$", ".xpt", results_df$program)


  # 6. reorder columns
  list_adams <- results_df %>% select(program, dataset, functions)

  # Print the final combined dataframe
  return(list_adams)
}
