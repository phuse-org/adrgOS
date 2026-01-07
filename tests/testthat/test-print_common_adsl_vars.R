test_that("print.common_adsl_vars works with valid data", {
    # Create test data
    test_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(test_data)
    class(result) <- c("common_adsl_vars", class(result))

    # Capture output
    output <- capture.output(print(result))

    # Check that output contains expected elements
    expect_true(any(grepl("COMMON ADSL VARIABLES ACROSS DOMAINS", output)))
    expect_true(any(grepl("ADSL Dataset:", output)))
    expect_true(any(grepl("Total ADSL Variables:", output)))
    expect_true(any(grepl("Common Variables Found:", output)))
    expect_true(any(grepl("Variables by Data Type:", output)))

    # Check that variable names appear in output
    expect_true(any(grepl("STUDYID", output)))
    expect_true(any(grepl("USUBJID", output)))
})

test_that("print.common_adsl_vars handles empty data", {
    # Create empty result
    empty_result <- data.frame()
    class(empty_result) <- c("common_adsl_vars", class(empty_result))

    output <- capture.output(print(empty_result))

    expect_true(any(grepl("No common variables found", output)))
})

test_that("print.common_adsl_vars handles data with many variables", {
    # Create test data with many variables
    test_data <- data.frame(
        Name = c(
            "STUDYID",
            "USUBJID",
            "AGE",
            "SEX",
            "RACE",
            "ARM",
            "TRT01P",
            "STUDYID",
            "USUBJID",
            "AGE",
            "SEX",
            "RACE",
            "ARM",
            "AVAL",
            "STUDYID",
            "USUBJID",
            "AGE",
            "SEX",
            "RACE",
            "ARM",
            "PARAMCD"
        ),
        Dataset = c(rep("ADSL", 7), rep("ADAE", 7), rep("ADLBC", 7)),
        DataType = c(rep("text", 7), rep("text", 7), rep("text", 7)),
        Length = c(rep("12", 7), rep("12", 7), rep("12", 7)),
        Description = c(
            rep("Description", 7),
            rep("Description", 7),
            rep("Description", 7)
        ),
        Origin = c(rep("Assigned", 7), rep("Assigned", 7), rep("Assigned", 7)),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(test_data)
    class(result) <- c("common_adsl_vars", class(result))

    output <- capture.output(print(result))

    # Should show top 5 variables section
    expect_true(any(grepl("Top 5 Most Widely Used Variables", output)))
})

test_that("print.common_adsl_vars handles non-data.frame input", {
    # Test with non-data.frame
    not_df <- list(a = 1, b = 2)
    class(not_df) <- c("common_adsl_vars", class(not_df))

    output <- capture.output(result <- print(not_df))

    # Should fall back to default print
    expect_true(any(grepl("\\$a", output)))
    expect_true(any(grepl("\\$b", output)))
})

test_that("print.common_adsl_vars shows correct data type summary", {
    # Create test data with mixed data types
    test_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "AGE", "STUDYID", "USUBJID", "AGE"),
        Dataset = c("ADSL", "ADSL", "ADSL", "ADAE", "ADAE", "ADAE"),
        DataType = c("text", "text", "integer", "text", "text", "integer"),
        Length = c("12", "20", "3", "12", "20", "3"),
        Description = c(
            "Study ID",
            "Subject ID",
            "Age",
            "Study ID",
            "Subject ID",
            "Age"
        ),
        Origin = c(
            "Assigned",
            "Assigned",
            "Collected",
            "Assigned",
            "Assigned",
            "Collected"
        ),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(test_data)
    class(result) <- c("common_adsl_vars", class(result))

    output <- capture.output(print(result))

    # Check data type summary
    expect_true(any(grepl("Variables by Data Type:", output)))
    expect_true(any(grepl("text:", output)))
    expect_true(any(grepl("integer:", output)))
})

test_that("print.common_adsl_vars shows attributes correctly", {
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
    class(result) <- c("common_adsl_vars", class(result))

    output <- capture.output(print(result))

    # Check that attributes are displayed
    expect_true(any(grepl("ADSL Dataset: ADSL", output)))
    expect_true(any(grepl("Total ADSL Variables: 2", output)))
    expect_true(any(grepl("Common Variables Found: 2", output)))
    expect_true(any(grepl("Minimum Domain Count: 1", output)))
    expect_true(any(grepl("Extraction Time:", output)))
})

test_that("print.common_adsl_vars handles missing attributes gracefully", {
    # Create a data frame without attributes
    result <- data.frame(
        Variable = c("STUDYID", "USUBJID"),
        DataType = c("text", "text"),
        Length = c("12", "20"),
        Description = c("Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned"),
        CodelistOID = c("", ""),
        DomainsCount = c(1, 1),
        DomainsFound = c("ADAE", "ADAE"),
        stringsAsFactors = FALSE
    )
    class(result) <- c("common_adsl_vars", class(result))

    output <- capture.output(print(result))

    # Should handle missing attributes gracefully
    expect_true(any(grepl("ADSL Dataset: Unknown", output)))
    expect_true(any(grepl("Total ADSL Variables: Unknown", output)))
    expect_true(any(grepl("Minimum Domain Count: 1", output))) # Default value
})

test_that("print method returns invisibly", {
    test_data <- data.frame(
        Name = c("STUDYID", "USUBJID", "STUDYID", "USUBJID"),
        Dataset = c("ADSL", "ADSL", "ADAE", "ADAE"),
        DataType = c("text", "text", "text", "text"),
        Length = c("12", "20", "12", "20"),
        Description = c("Study ID", "Subject ID", "Study ID", "Subject ID"),
        Origin = c("Assigned", "Assigned", "Assigned", "Assigned"),
        stringsAsFactors = FALSE
    )

    result <- extract_common_adsl_variables(test_data)
    class(result) <- c("common_adsl_vars", class(result))

    # Check that print returns the object invisibly
    printed_result <- print(result)
    expect_identical(printed_result, result)
})
