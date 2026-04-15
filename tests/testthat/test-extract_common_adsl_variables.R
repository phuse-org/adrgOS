test_that("extract_common_adsl_variables works with valid input", {
    # Create test data
    test_data <- data.frame(
        Name = c(
            "STUDYID",
            "USUBJID",
            "AGE",
            "SEX",
            "STUDYID",
            "USUBJID",
            "AVAL",
            "STUDYID",
            "USUBJID",
            "PARAMCD"
        ),
        Dataset = c(
            "ADSL",
            "ADSL",
            "ADSL",
            "ADSL",
            "ADAE",
            "ADAE",
            "ADAE",
            "ADLBC",
            "ADLBC",
            "ADLBC"
        ),
        DataType = c(
            "text",
            "text",
            "integer",
            "text",
            "text",
            "text",
            "float",
            "text",
            "text",
            "text"
        ),
        Length = c("12", "20", "3", "1", "12", "20", "8", "12", "20", "8"),
        Description = c(
            "Study Identifier",
            "Unique Subject ID",
            "Age",
            "Sex",
            "Study Identifier",
            "Unique Subject ID",
            "Analysis Value",
            "Study Identifier",
            "Unique Subject ID",
            "Parameter Code"
        ),
        Origin = c(
            "Assigned",
            "Assigned",
            "Collected",
            "Collected",
            "Assigned",
            "Assigned",
            "Derived",
            "Assigned",
            "Assigned",
            "Derived"
        ),
        stringsAsFactors = FALSE
    )

    # Test basic functionality
    result <- extract_common_adsl_variables(test_data)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true(all(
        c(
            "Variable",
            "DataType",
            "Description",
            "Origin",
            "DomainsCount",
            "DomainsFound"
        ) %in%
            names(result)
    ))

    # Check that common variables are found
    expect_true("STUDYID" %in% result$Variable)
    expect_true("USUBJID" %in% result$Variable)

    # Check domain counts
    studyid_row <- result[result$Variable == "STUDYID", ]
    expect_equal(studyid_row$DomainsCount, 2) # ADAE and ADLBC

    usubjid_row <- result[result$Variable == "USUBJID", ]
    expect_equal(usubjid_row$DomainsCount, 2) # ADAE and ADLBC
})

test_that("extract_common_adsl_variables handles SAS info parameter", {
    test_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        SASFieldName = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        stringsAsFactors = FALSE
    )

    # Test with SAS info included
    result_with_sas <- extract_common_adsl_variables(
        test_data,
        include_sas_info = TRUE
    )
    expect_true("SASFieldName" %in% names(result_with_sas))
    expect_true("SASLength" %in% names(result_with_sas))

    # Test with SAS info excluded
    result_without_sas <- extract_common_adsl_variables(
        test_data,
        include_sas_info = FALSE
    )
    expect_false("SASFieldName" %in% names(result_without_sas))
    expect_false("SASLength" %in% names(result_without_sas))
})

test_that("extract_common_adsl_variables handles min_domains_count parameter", {
    test_data <- data.frame(
        Name = c(
            "STUDYID",
            "USUBJID",
            "AGE",
            "STUDYID",
            "USUBJID",
            "AVAL",
            "STUDYID",
            "USUBJID",
            "PARAMCD",
            "STUDYID",
            "TRTGRP"
        ),
        Dataset = c(
            "ADSL",
            "ADSL",
            "ADSL",
            "ADAE",
            "ADAE",
            "ADAE",
            "ADLBC",
            "ADLBC",
            "ADLBC",
            "ADEFF",
            "ADEFF"
        ),
        DataType = c(
            "text",
            "text",
            "integer",
            "text",
            "text",
            "float",
            "text",
            "text",
            "text",
            "text",
            "text"
        ),
        Length = c(
            "12",
            "20",
            "3",
            "12",
            "20",
            "8",
            "12",
            "20",
            "8",
            "12",
            "20"
        ),
        Description = c(
            "Study ID",
            "Subject ID",
            "Age",
            "Study ID",
            "Subject ID",
            "Analysis Value",
            "Study ID",
            "Subject ID",
            "Parameter Code",
            "Study ID",
            "Treatment Group"
        ),
        Origin = c(
            "Assigned",
            "Assigned",
            "Collected",
            "Assigned",
            "Assigned",
            "Derived",
            "Assigned",
            "Assigned",
            "Derived",
            "Assigned",
            "Derived"
        ),
        stringsAsFactors = FALSE
    )

    # Test min_domains_count = 1
    result_min1 <- extract_common_adsl_variables(
        test_data,
        min_domains_count = 1
    )
    expect_gt(nrow(result_min1), 0)

    # Test min_domains_count = 2
    result_min2 <- extract_common_adsl_variables(
        test_data,
        min_domains_count = 2
    )
    expect_gt(nrow(result_min2), 0)
    # STUDYID and USUBJID should appear in 3 domains (ADAE, ADLBC, ADEFF)
    expect_true("STUDYID" %in% result_min2$Variable)
    expect_true("USUBJID" %in% result_min2$Variable)

    # Test min_domains_count = 4 (should return empty or warning)
    expect_warning(
        result_min4 <- extract_common_adsl_variables(
            test_data,
            min_domains_count = 4
        )
    )
    expect_equal(nrow(result_min4), 0)
})

test_that("extract_common_adsl_variables handles sort_by parameter", {
    test_data <- data.frame(
        Name = c(
            "STUDYID",
            "USUBJID",
            "AGE",
            "STUDYID",
            "USUBJID",
            "AVAL",
            "STUDYID",
            "PARAMCD"
        ),
        Dataset = c(
            "ADSL",
            "ADSL",
            "ADSL",
            "ADAE",
            "ADAE",
            "ADAE",
            "ADLBC",
            "ADLBC"
        ),
        DataType = c(
            "text",
            "text",
            "integer",
            "text",
            "text",
            "float",
            "text",
            "text"
        ),
        Length = c("12", "20", "3", "12", "20", "8", "12", "8"),
        Description = c(
            "Study ID",
            "Subject ID",
            "Age",
            "Study ID",
            "Subject ID",
            "Analysis Value",
            "Study ID",
            "Parameter Code"
        ),
        Origin = c(
            "Assigned",
            "Assigned",
            "Collected",
            "Assigned",
            "Assigned",
            "Derived",
            "Assigned",
            "Derived"
        ),
        stringsAsFactors = FALSE
    )

    # Test sort by domains_count (default)
    result_domains <- extract_common_adsl_variables(
        test_data,
        sort_by = "domains_count"
    )
    expect_equal(result_domains$Variable[1], "STUDYID") # Should be first (appears in 2 domains)

    # Test sort by variable_name
    result_name <- extract_common_adsl_variables(
        test_data,
        sort_by = "variable_name"
    )
    expect_equal(result_name$Variable[1], "STUDYID") # Alphabetically first
    expect_equal(result_name$Variable[2], "USUBJID") # Alphabetically second

    # Test sort by data_type
    result_type <- extract_common_adsl_variables(
        test_data,
        sort_by = "data_type"
    )
    expect_true(all(result_type$DataType == "text")) # All should be text in this example
})

test_that("extract_common_adsl_variables validates input parameters", {
    valid_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    # Test non-data.frame input
    expect_error(extract_common_adsl_variables("not_a_dataframe"))
    expect_error(extract_common_adsl_variables(list()))

    # Test missing required columns
    incomplete_data <- valid_data[, -1] # Remove Name column
    expect_error(extract_common_adsl_variables(incomplete_data))

    # Test invalid adsl_dataset_name
    expect_error(extract_common_adsl_variables(
        valid_data,
        adsl_dataset_name = c("ADSL", "ADAE")
    ))
    expect_error(extract_common_adsl_variables(
        valid_data,
        adsl_dataset_name = 123
    ))
    expect_error(extract_common_adsl_variables(
        valid_data,
        adsl_dataset_name = "NONEXISTENT"
    ))

    # Test invalid include_sas_info
    expect_error(extract_common_adsl_variables(
        valid_data,
        include_sas_info = "TRUE"
    ))
    expect_error(extract_common_adsl_variables(
        valid_data,
        include_sas_info = 1
    ))

    # Test invalid min_domains_count
    expect_error(extract_common_adsl_variables(
        valid_data,
        min_domains_count = "1"
    ))
    expect_error(extract_common_adsl_variables(
        valid_data,
        min_domains_count = 0
    ))
    expect_error(extract_common_adsl_variables(
        valid_data,
        min_domains_count = -1
    ))

    # Test invalid sort_by
    expect_error(extract_common_adsl_variables(
        valid_data,
        sort_by = "invalid_option"
    ))
    expect_error(extract_common_adsl_variables(valid_data, sort_by = 123))
})

test_that("extract_common_adsl_variables handles edge cases", {
    # Test with no ADSL variables
    no_adsl_data <- data.frame(
        Name = c("AVAL", "PARAMCD"),
        Dataset = c("ADAE", "ADAE"),
        DataType = c("float", "text"),
        Length = c("8", "8"),
        Description = c("Analysis Value", "Parameter Code"),
        Origin = c("Derived", "Derived"),
        stringsAsFactors = FALSE
    )
    expect_error(
        extract_common_adsl_variables(no_adsl_data),
        "Dataset ADSL not found in variable_info_df"
    )

    # Test with only ADSL variables (no other domains)
    only_adsl_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "AGE"),
        Dataset = c("ADSL", "ADSL", "ADSL"),
        DataType = c("text", "text", "integer"),
        Length = c("12", "20", "3"),
        Description = c("Study ID", "Subject ID", "Age"),
        Origin = c("Assigned", "Assigned", "Collected"),
        stringsAsFactors = FALSE
    )
    expect_warning(result <- extract_common_adsl_variables(only_adsl_data))
    expect_equal(nrow(result), 0)

    # Test with no common variables
    no_common_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "AVAL", "PARAMCD"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "float", "text"),
        Length = c("12", "20", "8", "8"),
        Description = c(
            "Study ID",
            "Subject ID",
            "Analysis Value",
            "Parameter Code"
        ),
        Origin = c("Assigned", "Assigned", "Derived", "Derived"),
        stringsAsFactors = FALSE
    )
    expect_warning(result <- extract_common_adsl_variables(no_common_data))
    expect_equal(nrow(result), 0)
})

test_that("extract_common_adsl_variables handles missing optional columns", {
    # Test with missing CodelistOID column
    data_no_codelist <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(data_no_codelist)
    expect_s3_class(result, "data.frame")
    expect_true("CodelistOID" %in% names(result))
    expect_true(all(result$CodelistOID == ""))

    # Test with missing SASFieldName column
    data_no_sas <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(
        data_no_sas,
        include_sas_info = TRUE
    )
    expect_s3_class(result, "data.frame")
    expect_true("SASFieldName" %in% names(result))
    expect_equal(result$SASFieldName, result$Variable) # Should default to Variable name
})

test_that("extract_common_adsl_variables result has correct attributes", {
    test_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(test_data, min_domains_count = 1)

    # Check attributes
    expect_equal(attr(result, "adsl_dataset"), "ADSL")
    expect_equal(attr(result, "total_adsl_vars"), 2)
    expect_equal(attr(result, "min_domains_count"), 1)
    expect_s3_class(attr(result, "extraction_time"), "POSIXct")
})
