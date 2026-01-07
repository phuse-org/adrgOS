# Extract Variable Information from Define.xml

This helper function parses a define.xml file to extract variable
metadata that can be used with extract_common_adsl_variables.

## Usage

``` r
extract_variable_info_from_define(define_path)
```

## Arguments

- define_path:

  Character string specifying the path to the define.xml file

## Value

A data.frame with columns: Name, Dataset, DataType, Length, Description,
Origin, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with included define.xml
define_path <- system.file("define.xml", package = "adrgOS")
if (file.exists(define_path)) {
  var_info <- extract_variable_info_from_define(define_path)
  print(head(var_info))

  # Use with extract_common_adsl_variables
  common_vars <- extract_common_adsl_variables(var_info)
  print(common_vars)
}

# Using local define.xml file
if (file.exists("inst/define.xml")) {
  var_info <- extract_variable_info_from_define("inst/define.xml")
  print(paste("Extracted", nrow(var_info), "variable definitions"))
}
} # }
```
