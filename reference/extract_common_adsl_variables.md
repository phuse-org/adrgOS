# Extract Common ADSL Variables Across Analysis Domains

This function identifies variables from the ADSL (Subject-Level Analysis
Dataset) that are also present in other analysis domains. It provides
comprehensive information about variable reuse and consistency across
study datasets, which is essential for data validation and understanding
variable provenance in clinical studies.

## Usage

``` r
extract_common_adsl_variables(
  variable_info_df,
  adsl_dataset_name = "ADSL",
  include_sas_info = TRUE,
  min_domains_count = 1,
  sort_by = "domains_count"
)
```

## Arguments

- variable_info_df:

  A data.frame containing variable information extracted from
  define.xml. Expected columns include: OID, Name, SASFieldName,
  DataType, Length, Description, Origin, CodelistOID, Dataset, and
  others.

- adsl_dataset_name:

  Character string specifying the name of the subject-level dataset.
  Default is "ADSL".

- include_sas_info:

  Logical indicating whether to include SAS-specific information in the
  output. Default is TRUE.

- min_domains_count:

  Integer specifying minimum number of domains a variable must appear in
  to be included in results. Default is 1.

- sort_by:

  Character string specifying how to sort results. Options are
  "domains_count" (default), "variable_name", or "data_type".

## Value

A data.frame with the following columns:

- Variable:

  Variable name from ADSL

- SASFieldName:

  SAS field name (if include_sas_info = TRUE)

- DataType:

  Data type (text, integer, float)

- Length:

  Variable length specification

- Description:

  Variable description from define.xml

- Origin:

  Variable origin (Predecessor, Derived, Assigned)

- CodelistOID:

  Reference to codelist if applicable

- DomainsCount:

  Number of domains containing this variable

- DomainsFound:

  Comma-separated list of domain names

- SASLength:

  SAS length specification (if include_sas_info = TRUE)

## Details

The function performs the following steps:

1.  Validates input parameters and data structure

2.  Extracts variables from the specified ADSL dataset

3.  Identifies matching variables in other analysis domains

4.  Counts domain occurrences for each common variable

5.  Formats and sorts results according to specified criteria

Common ADSL variables typically include:

- Study identifiers (STUDYID, USUBJID, SUBJID)

- Demographics (AGE, SEX, RACE, etc.)

- Treatment assignments (ARM, TRT01P, TRT01A, etc.)

- Study dates (TRTSDT, TRTEDT, etc.)

- Baseline characteristics

## See also

[`extract_variable_info_from_define`](https://phuse-org.github.io/adrgOS/reference/extract_variable_info_from_define.md)
for extracting variable information from define.xml

## Author

Clinical Data Science Team

## Examples

``` r
if (FALSE) { # \dontrun{
# Using the included define.xml file (recommended)
define_path <- system.file("define.xml", package = "adrgOS")
if (file.exists(define_path)) {
  variable_info <- extract_variable_info_from_define(define_path)
  common_vars <- extract_common_adsl_variables(variable_info)
  print(common_vars)

  # Filter by minimum domain count
  high_freq_vars <- extract_common_adsl_variables(
    variable_info,
    min_domains_count = 3
  )
  print(high_freq_vars)
}

# Alternative: Using custom sample data
sample_data <- data.frame(
  Name = c("STUDYID", "USUBJID", "AGE", "SEX", "STUDYID", "USUBJID", "AVAL",
           "STUDYID", "USUBJID", "PARAMCD"),
  Dataset = c("ADSL", "ADSL", "ADSL", "ADSL", "ADAE", "ADAE", "ADAE",
              "ADLBC", "ADLBC", "ADLBC"),
  DataType = c("text", "text", "integer", "text", "text", "text", "float",
               "text", "text", "text"),
  Length = c("12", "20", "3", "1", "12", "20", "8", "12", "20", "8"),
  Description = c("Study Identifier", "Unique Subject ID", "Age", "Sex",
                  "Study Identifier", "Unique Subject ID", "Analysis Value",
                  "Study Identifier", "Unique Subject ID", "Parameter Code"),
  Origin = c("Assigned", "Assigned", "Collected", "Collected",
             "Assigned", "Assigned", "Derived", "Assigned", "Assigned", "Derived"),
  stringsAsFactors = FALSE
)

# Basic usage
result <- extract_common_adsl_variables(sample_data)
print(result)

# Filter by minimum domain count (now works with improved sample data)
result_filtered <- extract_common_adsl_variables(
  sample_data,
  min_domains_count = 2
)
print(result_filtered)

# Sort by variable name without SAS info
result_sorted <- extract_common_adsl_variables(
  sample_data,
  include_sas_info = FALSE,
  sort_by = "variable_name"
)
print(result_sorted)
} # }
```
