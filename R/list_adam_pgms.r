# lists ADaM programs for esub package
#
list_adam_pgms <- function(path = ".", full_names = FALSE, include_hidden = FALSE) {

  # # Check if the provided path exists
  # if (!file.exists(path)) {
  #   stop(paste("Error: The specified path does not exist:", path))
  # }
  #
  # # Check if the provided path is a directory
  # if (!file.info(path)$isdir) {
  #   stop(paste("Error: The specified path is not a directory:", path))
  # }
  #
  # # List files using list.files()
  # # all.files argument controls inclusion of hidden files
  # # full.names argument controls whether full paths are returned
  # files <- list.files(
  #   path = path,
  #   all.files = include_hidden,
  #   full.names = full_names
  # )
  #
  # return(files)

  # 1. Show the files in that path and perform initial checks
  if (!file.exists(path)) {
    stop(paste("Error: The specified path does not exist:", path))
  }
  if (!file.info(path)$isdir) {
    stop(paste("Error: The specified path is not a directory:", path))
  }

  # List all files in the directory (including full paths if requested)
  all_files_in_path <- list.files(
    path = path,
    all.files = include_hidden,
    full.names = TRUE # Always get full names internally for reading files
  )

  # 2. Find all R programs that only start with "ad" and end with ".r" or ".R"
  # Use regex to match files starting with "ad" and ending with ".r" or ".R"
  r_programs_full_path <- grep(
    pattern = "^ad.*\\.[rR]$",
    x = basename(all_files_in_path), # Check basename for pattern match
    value = TRUE
  )

  # Reconstruct full paths for the filtered R programs
  r_programs_full_path <- all_files_in_path[basename(all_files_in_path) %in% r_programs_full_path]


  # 3. Put that list in alphabetical order in a dataframe column.
  # Get just the basenames for the primary column, sorted
  r_program_names_sorted <- sort(basename(r_programs_full_path))

  # Create the initial dataframe
  results_df <- data.frame(
    R_Program_Name = r_program_names_sorted,
    stringsAsFactors = FALSE
  )

  # 4. Then add another column, which copies the list from the first column,
  #    but then changes the suffix to ".xpt" instead.
  results_df$XPT_Equivalent <- gsub("\\.[rR]$", ".xpt", results_df$R_Program_Name)

  return(results_df)
}


