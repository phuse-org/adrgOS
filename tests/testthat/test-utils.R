test_that("null coalescing operator works correctly", {
    # Test with NULL values
    expect_equal(NULL %||% "default", "default")
    expect_equal(NULL %||% 42, 42)
    expect_equal(NULL %||% c(1, 2, 3), c(1, 2, 3))

    # Test with non-NULL values
    expect_equal("value" %||% "default", "value")
    expect_equal(42 %||% 0, 42)
    expect_equal(c(1, 2, 3) %||% c(4, 5, 6), c(1, 2, 3))

    # Test with empty values (should NOT be coalesced)
    expect_equal("" %||% "default", "")
    expect_equal(0 %||% 42, 0)
    expect_equal(character(0) %||% "default", character(0))

    # Test with NA values (should NOT be coalesced)
    expect_equal(NA %||% "default", NA)
    expect_equal(NA_character_ %||% "default", NA_character_)
    expect_equal(NA_real_ %||% 42, NA_real_)

    # Test chaining
    expect_equal(NULL %||% NULL %||% "final", "final")
    expect_equal("first" %||% NULL %||% "final", "first")
    expect_equal(NULL %||% "second" %||% "final", "second")
})

test_that("null coalescing operator handles different data types", {
    # Test with different data types
    expect_equal(NULL %||% TRUE, TRUE)
    expect_equal(NULL %||% FALSE, FALSE)
    expect_equal(NULL %||% list(a = 1, b = 2), list(a = 1, b = 2))
    expect_equal(NULL %||% data.frame(x = 1, y = 2), data.frame(x = 1, y = 2))

    # Test with functions
    test_func <- function(x) x + 1
    expect_equal(NULL %||% test_func, test_func)

    # Test with complex objects
    test_env <- new.env()
    test_env$a <- 1
    expect_equal(NULL %||% test_env, test_env)
})

test_that("null coalescing operator is used correctly in main function", {
    # Test that %||% is used in the XML parsing function
    test_xml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">
  <Study>
    <MetaDataVersion>
      <ItemGroupDef OID="IG.ADSL" Name="ADSL">
        <ItemRef ItemOID="IT.ADSL.STUDYID" Mandatory="Yes"/>
      </ItemGroupDef>
      <ItemDef OID="IT.ADSL.STUDYID" Name="STUDYID" DataType="text">
        <!-- Missing Length and SASFieldName attributes -->
      </ItemDef>
    </MetaDataVersion>
  </Study>
</ODM>'

    skip_if_not_installed("xml2")

    temp_xml <- tempfile(fileext = ".xml")
    writeLines(test_xml_content, temp_xml)

    result <- extract_variable_info_from_define(temp_xml)

    unlink(temp_xml)

    # Check that missing attributes are handled with default values
    expect_equal(result$Length, "") # Should default to ""
    expect_equal(result$SASFieldName, "STUDYID") # Should default to Name
    expect_equal(result$CodelistOID, "") # Should default to ""
})
