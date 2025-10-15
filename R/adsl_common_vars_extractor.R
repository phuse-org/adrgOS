#' Helper function for null coalescing
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Extract Common ADSL Variables Across Analysis Domains
#'
#' This function identifies variables from the ADSL (Subject-Level Analysis Dataset)
#' that are also present in other analysis domains. It provides comprehensive
#' information about variable reuse and consistency across study datasets, which is
#' essential for data validation and understanding variable provenance in clinical studies.
#'
#' @param variable_info_df A data.frame containing variable information extracted from
#'   define.xml. Expected columns include: OID, Name, SASFieldName, DataType, Length,
#'   Description, Origin, CodelistOID, Dataset, and others.
#' @param adsl_dataset_name Character string specifying the name of the subject-level
#'   dataset. Default is "ADSL".
#' @param include_sas_info Logical indicating whether to include SAS-specific
#'   information in the output. Default is TRUE.
#' @param min_domains_count Integer specifying minimum number of domains a variable
#'   must appear in to be included in results. Default is 1.
#' @param sort_by Character string specifying how to sort results. Options are
#'   "domains_count" (default), "variable_name", or "data_type".
#'
#' @return A data.frame with the following columns:
#'   \describe{
#'     \item{Variable}{Variable name from ADSL}
#'     \item{SASFieldName}{SAS field name (if include_sas_info = TRUE)}
#'     \item{DataType}{Data type (text, integer, float)}
#'     \item{Length}{Variable length specification}
#'     \item{Description}{Variable description from define.xml}
#'     \item{Origin}{Variable origin (Predecessor, Derived, Assigned)}
#'     \item{CodelistOID}{Reference to codelist if applicable}
#'     \item{DomainsCount}{Number of domains containing this variable}
#'     \item{DomainsFound}{Comma-separated list of domain names}
#'     \item{SASLength}{SAS length specification (if include_sas_info = TRUE)}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input parameters and data structure
#'   \item Extracts variables from the specified ADSL dataset
#'   \item Identifies matching variables in other analysis domains
#'   \item Counts domain occurrences for each common variable
#'   \item Formats and sorts results according to specified criteria
#' }
#'
#' Common ADSL variables typically include:
#' \itemize{
#'   \item Study identifiers (STUDYID, USUBJID, SUBJID)
#'   \item Demographics (AGE, SEX, RACE, etc.)
#'   \item Treatment assignments (ARM, TRT01P, TRT01A, etc.)
#'   \item Study dates (TRTSDT, TRTEDT, etc.)
#'   \item Baseline characteristics
#' }
#'
#' @examples
#' \dontrun{
#' # Using the included define.xml file (recommended)
#' define_path <- system.file("define.xml", package = "adrgOS")
#' if (file.exists(define_path)) {
#'   variable_info <- extract_variable_info_from_define(define_path)
#'   common_vars <- extract_common_adsl_variables(variable_info)
#'   print(common_vars)
#'
#'   # Filter by minimum domain count
#'   high_freq_vars <- extract_common_adsl_variables(
#'     variable_info,
#'     min_domains_count = 3
#'   )
#'   print(high_freq_vars)
#' }
#'
#' # Alternative: Using custom sample data
#' sample_data <- data.frame(
#'   Name = c("STUDYID", "USUBJID", "AGE", "SEX", "STUDYID", "USUBJID", "AVAL",
#'            "STUDYID", "USUBJID", "PARAMCD"),
#'   Dataset = c("ADSL", "ADSL", "ADSL", "ADSL", "ADAE", "ADAE", "ADAE",
#'               "ADLBC", "ADLBC", "ADLBC"),
#'   DataType = c("text", "text", "integer", "text", "text", "text", "float",
#'                "text", "text", "text"),
#'   Length = c("12", "20", "3", "1", "12", "20", "8", "12", "20", "8"),
#'   Description = c("Study Identifier", "Unique Subject ID", "Age", "Sex",
#'                   "Study Identifier", "Unique Subject ID", "Analysis Value",
#'                   "Study Identifier", "Unique Subject ID", "Parameter Code"),
#'   Origin = c("Assigned", "Assigned", "Collected", "Collected",
#'              "Assigned", "Assigned", "Derived", "Assigned", "Assigned", "Derived"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic usage
#' result <- extract_common_adsl_variables(sample_data)
#' print(result)
#'
#' # Filter by minimum domain count (now works with improved sample data)
#' result_filtered <- extract_common_adsl_variables(
#'   sample_data,
#'   min_domains_count = 2
#' )
#' print(result_filtered)
#'
#' # Sort by variable name without SAS info
#' result_sorted <- extract_common_adsl_variables(
#'   sample_data,
#'   include_sas_info = FALSE,
#'   sort_by = "variable_name"
#' )
#' print(result_sorted)
#' }
#'
#' @seealso
#' \code{\link{extract_variable_info_from_define}} for extracting variable information from define.xml
#'
#' @author Clinical Data Science Team
#' @export
extract_common_adsl_variables <- function(
  variable_info_df,
  adsl_dataset_name = "ADSL",
  include_sas_info = TRUE,
  min_domains_count = 1,
  sort_by = "domains_count"
) {
  # Input validation
  if (!is.data.frame(variable_info_df)) {
    stop("variable_info_df must be a data.frame")
  }

  required_cols <- c("Name", "Dataset", "DataType", "Description", "Origin")
  missing_cols <- setdiff(required_cols, names(variable_info_df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  if (!is.character(adsl_dataset_name) || length(adsl_dataset_name) != 1) {
    stop("adsl_dataset_name must be a single character string")
  }

  if (!adsl_dataset_name %in% variable_info_df$Dataset) {
    stop(paste("Dataset", adsl_dataset_name, "not found in variable_info_df"))
  }

  if (!is.logical(include_sas_info)) {
    stop("include_sas_info must be logical (TRUE/FALSE)")
  }

  if (!is.numeric(min_domains_count) || min_domains_count < 1) {
    stop("min_domains_count must be a positive integer")
  }

  valid_sort_options <- c("domains_count", "variable_name", "data_type")
  if (!sort_by %in% valid_sort_options) {
    stop(paste(
      "sort_by must be one of:",
      paste(valid_sort_options, collapse = ", ")
    ))
  }

  # Extract ADSL variables
  adsl_vars <- variable_info_df[variable_info_df$Dataset == adsl_dataset_name, ]

  if (nrow(adsl_vars) == 0) {
    warning(paste("No variables found in dataset:", adsl_dataset_name))
    return(data.frame())
  }

  # Extract variables from other domains
  other_domains <- variable_info_df[
    variable_info_df$Dataset != adsl_dataset_name,
  ]

  if (nrow(other_domains) == 0) {
    warning("No other domains found for comparison")
    return(data.frame())
  }

  # Find common variables by name
  common_vars <- adsl_vars[adsl_vars$Name %in% other_domains$Name, ]

  if (nrow(common_vars) == 0) {
    warning("No common variables found between ADSL and other domains")
    return(data.frame())
  }

  # Calculate domain counts and lists for each common variable
  domain_info <- lapply(common_vars$Name, function(var_name) {
    # Find all domains containing this variable (excluding ADSL)
    domains_with_var <- other_domains[other_domains$Name == var_name, "Dataset"]
    domains_with_var <- unique(domains_with_var)

    # Sort domains alphabetically
    domains_with_var <- sort(domains_with_var)

    list(
      count = length(domains_with_var),
      domains = paste(domains_with_var, collapse = ", ")
    )
  })

  # Extract counts and domain lists
  domain_counts <- sapply(domain_info, function(x) x$count)
  domain_lists <- sapply(domain_info, function(x) x$domains)

  # Build result data frame
  result_cols <- c(
    "Variable",
    "DataType",
    "Length",
    "Description",
    "Origin",
    "CodelistOID",
    "DomainsCount",
    "DomainsFound"
  )

  if (include_sas_info) {
    result_cols <- c("Variable", "SASFieldName", result_cols[-1], "SASLength")
  }

  # Create base result data frame
  result_df <- data.frame(
    Variable = common_vars$Name,
    DataType = common_vars$DataType,
    Length = ifelse(
      is.na(common_vars$Length) | common_vars$Length == "",
      "Not specified",
      common_vars$Length
    ),
    Description = ifelse(
      is.na(common_vars$Description) | common_vars$Description == "",
      "No description",
      common_vars$Description
    ),
    Origin = ifelse(
      is.na(common_vars$Origin) | common_vars$Origin == "",
      "Not specified",
      common_vars$Origin
    ),
    CodelistOID = ifelse(
      "CodelistOID" %in% names(common_vars),
      ifelse(
        is.na(common_vars$CodelistOID) | common_vars$CodelistOID == "",
        "",
        common_vars$CodelistOID
      ),
      ""
    ),
    DomainsCount = domain_counts,
    DomainsFound = domain_lists,
    stringsAsFactors = FALSE
  )

  # Add SAS-specific information if requested
  if (include_sas_info) {
    if ("SASFieldName" %in% names(common_vars)) {
      sas_field_name <- ifelse(
        is.na(common_vars$SASFieldName) | common_vars$SASFieldName == "",
        common_vars$Name,
        common_vars$SASFieldName
      )
    } else {
      sas_field_name <- common_vars$Name
    }

    # Create SAS length information
    sas_length <- paste0(
      common_vars$DataType,
      ifelse(
        common_vars$Length != "" & !is.na(common_vars$Length),
        paste0("(", common_vars$Length, ")"),
        ""
      )
    )

    result_df <- data.frame(
      Variable = result_df$Variable,
      SASFieldName = sas_field_name,
      DataType = result_df$DataType,
      Length = result_df$Length,
      Description = result_df$Description,
      Origin = result_df$Origin,
      CodelistOID = result_df$CodelistOID,
      DomainsCount = result_df$DomainsCount,
      DomainsFound = result_df$DomainsFound,
      SASLength = sas_length,
      stringsAsFactors = FALSE
    )
  }

  # Filter by minimum domains count
  result_df <- result_df[result_df$DomainsCount >= min_domains_count, ]

  if (nrow(result_df) == 0) {
    warning(paste(
      "No variables found appearing in",
      min_domains_count,
      "or more domains"
    ))
    return(data.frame())
  }

  # Sort results
  if (sort_by == "domains_count") {
    result_df <- result_df[order(-result_df$DomainsCount, result_df$Variable), ]
  } else if (sort_by == "variable_name") {
    result_df <- result_df[order(result_df$Variable), ]
  } else if (sort_by == "data_type") {
    result_df <- result_df[order(result_df$DataType, result_df$Variable), ]
  }

  # Reset row names
  rownames(result_df) <- NULL

  # Add attributes for metadata
  attr(result_df, "adsl_dataset") <- adsl_dataset_name
  attr(result_df, "total_adsl_vars") <- nrow(adsl_vars)
  attr(result_df, "extraction_time") <- Sys.time()
  attr(result_df, "min_domains_count") <- min_domains_count

  return(result_df)
}

#' Print Method for Common ADSL Variables
#'
#' Custom print method for the output of extract_common_adsl_variables
#'
#' @param x Data frame returned by extract_common_adsl_variables
#' @param ... Additional arguments passed to print
#' @export
print.common_adsl_vars <- function(x, ...) {
  if (!is.data.frame(x)) {
    print.default(x, ...)
    return(invisible(x))
  }

  cat("=== COMMON ADSL VARIABLES ACROSS DOMAINS ===\n")
  cat(
    "ADSL Dataset:",
    attr(x, "adsl_dataset", exact = TRUE) %||% "Unknown",
    "\n"
  )
  cat(
    "Total ADSL Variables:",
    attr(x, "total_adsl_vars", exact = TRUE) %||% "Unknown",
    "\n"
  )
  cat("Common Variables Found:", nrow(x), "\n")
  cat(
    "Minimum Domain Count:",
    attr(x, "min_domains_count", exact = TRUE) %||% 1,
    "\n"
  )
  cat(
    "Extraction Time:",
    format(attr(x, "extraction_time", exact = TRUE) %||% Sys.time()),
    "\n"
  )
  cat("\n")

  if (nrow(x) > 0) {
    # Print summary by data type
    cat("Variables by Data Type:\n")
    type_summary <- table(x$DataType)
    for (i in seq_along(type_summary)) {
      cat(sprintf("  %s: %d\n", names(type_summary)[i], type_summary[i]))
    }
    cat("\n")

    # Print the data frame
    print.data.frame(x, ...)

    # Print top variables by domain count
    if (nrow(x) > 5) {
      cat("\nTop 5 Most Widely Used Variables:\n")
      top_vars <- head(
        x[
          order(-x$DomainsCount),
          c("Variable", "DomainsCount", "DomainsFound")
        ],
        5
      )
      print(top_vars, row.names = FALSE)
    }
  } else {
    cat("No common variables found.\n")
  }

  invisible(x)
}


#' Extract Variable Information from Define.xml
#'
#' This helper function parses a define.xml file to extract variable metadata
#' that can be used with extract_common_adsl_variables.
#'
#' @param define_path Character string specifying the path to the define.xml file
#' @return A data.frame with columns: Name, Dataset, DataType, Length, Description, Origin, etc.
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with included define.xml
#' define_path <- system.file("define.xml", package = "adrgOS")
#' if (file.exists(define_path)) {
#'   var_info <- extract_variable_info_from_define(define_path)
#'   print(head(var_info))
#'
#'   # Use with extract_common_adsl_variables
#'   common_vars <- extract_common_adsl_variables(var_info)
#'   print(common_vars)
#' }
#'
#' # Using local define.xml file
#' if (file.exists("inst/define.xml")) {
#'   var_info <- extract_variable_info_from_define("inst/define.xml")
#'   print(paste("Extracted", nrow(var_info), "variable definitions"))
#' }
#' }
extract_variable_info_from_define <- function(define_path) {
  if (!file.exists(define_path)) {
    stop("Define.xml file not found at: ", define_path)
  }

  # Load required package
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package 'xml2' is required. Please install it with: install.packages('xml2')"
    )
  }

  # Read the XML file
  xml_doc <- xml2::read_xml(define_path)

  # Use local-name() to avoid namespace issues
  item_defs <- xml2::xml_find_all(xml_doc, "//*[local-name()='ItemDef']")

  if (length(item_defs) == 0) {
    stop("No ItemDef elements found in define.xml")
  }

  # Find all ItemGroupDef elements (dataset definitions)
  item_group_defs <- xml2::xml_find_all(
    xml_doc,
    "//*[local-name()='ItemGroupDef']"
  )

  # Create a mapping of OID to dataset name
  dataset_mapping <- list()
  for (group_def in item_group_defs) {
    group_oid <- xml2::xml_attr(group_def, "OID")
    group_name <- xml2::xml_attr(group_def, "Name")
    if (!is.na(group_oid) && !is.na(group_name)) {
      dataset_mapping[[group_oid]] <- group_name
    }
  }

  # Extract variable information
  var_info <- data.frame()

  for (item_def in item_defs) {
    # Extract basic attributes
    oid <- xml2::xml_attr(item_def, "OID")
    if (is.na(oid)) {
      oid <- ""
    }

    name <- xml2::xml_attr(item_def, "Name")
    if (is.na(name)) {
      name <- ""
    }

    sas_field_name <- xml2::xml_attr(item_def, "SASFieldName")
    if (is.na(sas_field_name)) {
      sas_field_name <- name
    }

    data_type <- xml2::xml_attr(item_def, "DataType")
    if (is.na(data_type)) {
      data_type <- "text"
    }

    length_attr <- xml2::xml_attr(item_def, "Length")
    if (is.na(length_attr)) {
      length_attr <- ""
    }

    # Extract description
    description_node <- xml2::xml_find_first(
      item_def,
      ".//*[local-name()='Description']/*[local-name()='TranslatedText']"
    )
    description <- ifelse(
      is.na(description_node),
      "",
      xml2::xml_text(description_node)
    )

    # Extract origin
    origin_node <- xml2::xml_find_first(item_def, ".//*[local-name()='Origin']")
    origin <- ifelse(
      is.na(origin_node),
      "",
      xml2::xml_attr(origin_node, "Type")
    )

    # Extract codelist reference
    codelist_oid <- xml2::xml_attr(item_def, "CodeListOID")
    if (is.na(codelist_oid)) {
      codelist_oid <- ""
    }

    # Find which datasets contain this variable
    item_refs <- xml2::xml_find_all(
      xml_doc,
      paste0("//*[local-name()='ItemRef'][@ItemOID='", oid, "']")
    )

    # Get dataset names for this variable
    datasets <- character()
    for (item_ref in item_refs) {
      parent_group <- xml2::xml_parent(item_ref)
      if (!is.na(parent_group)) {
        parent_oid <- xml2::xml_attr(parent_group, "OID")
        if (!is.na(parent_oid) && parent_oid %in% names(dataset_mapping)) {
          datasets <- c(datasets, dataset_mapping[[parent_oid]])
        }
      }
    }

    # Create a row for each dataset containing this variable
    if (length(datasets) > 0) {
      for (dataset in datasets) {
        var_info <- rbind(
          var_info,
          data.frame(
            OID = oid,
            Name = name,
            SASFieldName = sas_field_name,
            DataType = data_type,
            Length = length_attr,
            Description = description,
            Origin = origin,
            CodelistOID = codelist_oid,
            Dataset = dataset,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  return(var_info)
}
