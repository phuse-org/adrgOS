# Extracting Common ADSL Variables Across Domains

## Introduction

In clinical data analysis, the ADSL (Subject-Level Analysis Dataset)
often contains critical variables that are reused across various
analysis domains (e.g., ADAE, ADLB, ADLBC). The
[`extract_common_adsl_variables()`](https://phuse-org.github.io/adrgOS/reference/extract_common_adsl_variables.md)
function in the **adrgOS** package helps identify and summarize these
common variables, providing insights into variable provenance and
consistency across domains.

## Installation

You can install **adrgOS** from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("phuse-org/adrgOS")
```

## Usage

### Extracting variable information from define.xml

First, extract variable metadata from your Define-XML file using
[`extract_variable_info_from_define()`](https://phuse-org.github.io/adrgOS/reference/extract_variable_info_from_define.md).
If you have a Define-XML file in your package (`inst/define.xml`), you
can locate it with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

``` r
library(adrgOS)
define_path <- system.file("define.xml", package = "adrgOS")
if (file.exists(define_path)) {
  variable_info <- extract_variable_info_from_define(define_path)
} else {
  stop("Define-XML file not found in the package.")
}
```

### Extract common ADSL variables

Use
[`extract_common_adsl_variables()`](https://phuse-org.github.io/adrgOS/reference/extract_common_adsl_variables.md)
to identify variables present in ADSL and other domains.

``` r
common_vars <- extract_common_adsl_variables(
  variable_info_df = variable_info,
  adsl_dataset_name = "ADSL",      # Name of the ADSL dataset
  include_sas_info    = TRUE,        # Include SAS field and length info
  min_domains_count   = 1,           # Minimum number of domains
  sort_by             = "domains_count"  # Sort by number of domains
)

# View the first few rows
head(common_vars)
#>   Variable SASFieldName DataType Length            Description  Origin
#> 1      AGE          AGE  integer      8                    Age Derived
#> 2   AGEGR1       AGEGR1     text      5     Pooled Age Group 1 Derived
#> 3  AGEGR1N      AGEGR1N  integer      8 Pooled Age Group 1 (N) Derived
#> 4     RACE         RACE     text     32                   Race Derived
#> 5    RACEN        RACEN  integer      8               Race (N) Derived
#> 6      SEX          SEX     text      1                    Sex Derived
#>   CodelistOID DomainsCount               DomainsFound  SASLength
#> 1                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 2                        4 ADADAS, ADAE, ADLBC, ADTTE    text(5)
#> 3                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 4                        4 ADADAS, ADAE, ADLBC, ADTTE   text(32)
#> 5                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 6                        4 ADADAS, ADAE, ADLBC, ADTTE    text(1)
```

## Customizing the results

#### Filtering by domain frequency

To restrict to variables that appear in at least 3 other domains:

``` r
high_freq_vars <- extract_common_adsl_variables(
  variable_info_df = variable_info,
  min_domains_count = 3
)
head(high_freq_vars)
#>   Variable SASFieldName DataType Length            Description  Origin
#> 1      AGE          AGE  integer      8                    Age Derived
#> 2   AGEGR1       AGEGR1     text      5     Pooled Age Group 1 Derived
#> 3  AGEGR1N      AGEGR1N  integer      8 Pooled Age Group 1 (N) Derived
#> 4     RACE         RACE     text     32                   Race Derived
#> 5    RACEN        RACEN  integer      8               Race (N) Derived
#> 6      SEX          SEX     text      1                    Sex Derived
#>   CodelistOID DomainsCount               DomainsFound  SASLength
#> 1                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 2                        4 ADADAS, ADAE, ADLBC, ADTTE    text(5)
#> 3                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 4                        4 ADADAS, ADAE, ADLBC, ADTTE   text(32)
#> 5                        4 ADADAS, ADAE, ADLBC, ADTTE integer(8)
#> 6                        4 ADADAS, ADAE, ADLBC, ADTTE    text(1)
```

#### Excluding SAS-specific information

If you prefer a simpler output without SAS field names and lengths, set
`include_sas_info = FALSE`:

``` r
vars_no_sas <- extract_common_adsl_variables(
  variable_info_df = variable_info,
  include_sas_info = FALSE,
  sort_by = "variable_name"
)
head(vars_no_sas)
#>   Variable DataType Length                           Description  Origin
#> 1      AGE  integer      8                                   Age Derived
#> 2   AGEGR1     text      5                    Pooled Age Group 1 Derived
#> 3  AGEGR1N  integer      8                Pooled Age Group 1 (N) Derived
#> 4 COMP24FL     text      1 Completers of Week 24 Population Flag Derived
#> 5  DSRAEFL     text      1               Discontinued due to AE? Derived
#> 6    EFFFL     text      1              Efficacy Population Flag Derived
#>   CodelistOID DomainsCount               DomainsFound
#> 1                        4 ADADAS, ADAE, ADLBC, ADTTE
#> 2                        4 ADADAS, ADAE, ADLBC, ADTTE
#> 3                        4 ADADAS, ADAE, ADLBC, ADTTE
#> 4                        2              ADADAS, ADLBC
#> 5                        1                      ADLBC
#> 6                        1                     ADADAS
```

## Working with sample data

When testing or demonstrating functionality, you can create a simple
sample data frame:

``` r
sample_data <- data.frame(
  Name = c("STUDYID", "USUBJID", "AGE", "SEX", 
           "STUDYID", "USUBJID", "AVAL", 
           "STUDYID", "USUBJID", "PARAMCD"),
  Dataset = c("ADSL", "ADSL", "ADSL", "ADSL", 
              "ADAE", "ADAE", "ADAE", 
              "ADLBC", "ADLBC", "ADLBC"),
  DataType = c("text", "text", "integer", "text",
               "text", "text", "float",
               "text", "text", "text"),
  Length = c("12", "20", "3", "1",
             "12", "20", "8",
             "12", "20", "8"),
  Description = c("Study Identifier", "Unique Subject ID", "Age", "Sex",
                  "Study Identifier", "Unique Subject ID", "Analysis Value",
                  "Study Identifier", "Unique Subject ID", "Parameter Code"),
  Origin = c("Assigned", "Assigned", "Collected", "Collected",
             "Assigned", "Assigned", "Derived", "Assigned", "Assigned", "Derived"),
  stringsAsFactors = FALSE
)

# Extract from sample data
result <- extract_common_adsl_variables(sample_data)
print(result)
#>   Variable SASFieldName DataType Length       Description   Origin CodelistOID
#> 1  STUDYID      STUDYID     text     12  Study Identifier Assigned            
#> 2  USUBJID      USUBJID     text     20 Unique Subject ID Assigned            
#>   DomainsCount DomainsFound SASLength
#> 1            2  ADAE, ADLBC  text(12)
#> 2            2  ADAE, ADLBC  text(20)
```

## Conclusion

The **adrgOS** vignette demonstrated how to use
[`extract_common_adsl_variables()`](https://phuse-org.github.io/adrgOS/reference/extract_common_adsl_variables.md)
to streamline the identification and analysis of common ADSL variables
across domains. This tool aids in verifying data consistency and
understanding variable reuse in clinical datasets.

## See Also

For more details on extracting Define-XML metadata, see
`?extract_variable_info_from_define()`.
