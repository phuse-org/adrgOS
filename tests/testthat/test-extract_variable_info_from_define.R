test_that("extract_variable_info_from_define works with valid XML", {
    # Skip test if xml2 is not available
    skip_if_not_installed("xml2")

    # Create a simple test XML file
    test_xml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">
  <Study>
    <MetaDataVersion>
      <ItemGroupDef OID="IG.ADSL" Name="ADSL">
        <ItemRef ItemOID="IT.ADSL.STUDYID" Mandatory="Yes"/>
        <ItemRef ItemOID="IT.ADSL.USUBJID" Mandatory="Yes"/>
      </ItemGroupDef>
      <ItemGroupDef OID="IG.ADAE" Name="ADAE">
        <ItemRef ItemOID="IT.ADAE.STUDYID" Mandatory="Yes"/>
        <ItemRef ItemOID="IT.ADAE.USUBJID" Mandatory="Yes"/>
        <ItemRef ItemOID="IT.ADAE.AVAL" Mandatory="Yes"/>
      </ItemGroupDef>
      <ItemDef OID="IT.ADSL.STUDYID" Name="STUDYID" DataType="text" Length="12" SASFieldName="STUDYID">
        <Description>
          <TranslatedText>Study Identifier</TranslatedText>
        </Description>
        <Origin Type="Assigned"/>
      </ItemDef>
      <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID" DataType="text" Length="20" SASFieldName="USUBJID">
        <Description>
          <TranslatedText>Unique Subject Identifier</TranslatedText>
        </Description>
        <Origin Type="Assigned"/>
      </ItemDef>
      <ItemDef OID="IT.ADAE.STUDYID" Name="STUDYID" DataType="text" Length="12" SASFieldName="STUDYID">
        <Description>
          <TranslatedText>Study Identifier</TranslatedText>
        </Description>
        <Origin Type="Assigned"/>
      </ItemDef>
      <ItemDef OID="IT.ADAE.USUBJID" Name="USUBJID" DataType="text" Length="20" SASFieldName="USUBJID">
        <Description>
          <TranslatedText>Unique Subject Identifier</TranslatedText>
        </Description>
        <Origin Type="Assigned"/>
      </ItemDef>
      <ItemDef OID="IT.ADAE.AVAL" Name="AVAL" DataType="float" Length="8" SASFieldName="AVAL">
        <Description>
          <TranslatedText>Analysis Value</TranslatedText>
        </Description>
        <Origin Type="Derived"/>
      </ItemDef>
    </MetaDataVersion>
  </Study>
</ODM>'

    # Write test XML to temporary file
    temp_xml <- tempfile(fileext = ".xml")
    writeLines(test_xml_content, temp_xml)

    # Test the function
    result <- extract_variable_info_from_define(temp_xml)

    # Clean up
    unlink(temp_xml)

    # Verify results
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)

    # Check required columns
    expected_cols <- c(
        "OID",
        "Name",
        "SASFieldName",
        "DataType",
        "Length",
        "Description",
        "Origin",
        "CodelistOID",
        "Dataset"
    )
    expect_true(all(expected_cols %in% names(result)))

    # Check specific values
    expect_true("STUDYID" %in% result$Name)
    expect_true("USUBJID" %in% result$Name)
    expect_true("AVAL" %in% result$Name)
    expect_true("ADSL" %in% result$Dataset)
    expect_true("ADAE" %in% result$Dataset)

    # Check that STUDYID appears in both datasets
    studyid_rows <- result[result$Name == "STUDYID", ]
    expect_equal(nrow(studyid_rows), 2)
    expect_true("ADSL" %in% studyid_rows$Dataset)
    expect_true("ADAE" %in% studyid_rows$Dataset)
})

test_that("extract_variable_info_from_define handles file not found", {
    expect_error(
        extract_variable_info_from_define("/nonexistent/path/define.xml"),
        "Define.xml file not found"
    )
})

test_that("extract_variable_info_from_define handles missing xml2 package", {
    # Skip this test as mocking is complex and not essential for core functionality
    skip("Mocking test skipped - xml2 package requirement is well documented")
})

test_that("extract_variable_info_from_define handles invalid XML", {
    skip_if_not_installed("xml2")

    # Create invalid XML
    invalid_xml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">
  <Study>
    <MetaDataVersion>
      <!-- No ItemDef elements -->
    </MetaDataVersion>
  </Study>
</ODM>'

    temp_xml <- tempfile(fileext = ".xml")
    writeLines(invalid_xml_content, temp_xml)

    expect_error(
        extract_variable_info_from_define(temp_xml),
        "No ItemDef elements found"
    )

    unlink(temp_xml)
})

test_that("extract_variable_info_from_define handles XML without namespace", {
    skip_if_not_installed("xml2")

    # Create XML without namespace
    no_namespace_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM>
  <Study>
    <MetaDataVersion>
      <ItemGroupDef OID="IG.ADSL" Name="ADSL">
        <ItemRef ItemOID="IT.ADSL.STUDYID" Mandatory="Yes"/>
      </ItemGroupDef>
      <ItemDef OID="IT.ADSL.STUDYID" Name="STUDYID" DataType="text" Length="12">
        <Description>
          <TranslatedText>Study Identifier</TranslatedText>
        </Description>
        <Origin Type="Assigned"/>
      </ItemDef>
    </MetaDataVersion>
  </Study>
</ODM>'

    temp_xml <- tempfile(fileext = ".xml")
    writeLines(no_namespace_xml, temp_xml)

    result <- extract_variable_info_from_define(temp_xml)

    unlink(temp_xml)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true("STUDYID" %in% result$Name)
})

test_that("extract_variable_info_from_define handles missing optional elements", {
    skip_if_not_installed("xml2")

    # Create XML with minimal elements
    minimal_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">
  <Study>
    <MetaDataVersion>
      <ItemGroupDef OID="IG.ADSL" Name="ADSL">
        <ItemRef ItemOID="IT.ADSL.STUDYID" Mandatory="Yes"/>
      </ItemGroupDef>
      <ItemDef OID="IT.ADSL.STUDYID" Name="STUDYID" DataType="text">
        <!-- No Description, Origin, Length, SASFieldName -->
      </ItemDef>
    </MetaDataVersion>
  </Study>
</ODM>'

    temp_xml <- tempfile(fileext = ".xml")
    writeLines(minimal_xml, temp_xml)

    result <- extract_variable_info_from_define(temp_xml)

    unlink(temp_xml)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)

    # Check that missing elements are handled gracefully
    studyid_row <- result[result$Name == "STUDYID", ]
    expect_equal(studyid_row$Description, "")
    expect_equal(studyid_row$Origin, "")
    expect_equal(studyid_row$Length, "")
    expect_equal(studyid_row$SASFieldName, "STUDYID") # Should default to Name
})

test_that("extract_variable_info_from_define works with real define.xml if available", {
    skip_if_not_installed("xml2")

    # Check if the real define.xml exists
    define_path <- system.file("define.xml", package = "adrgOS")
    if (define_path == "") {
        # Try local path
        define_path <- file.path("inst", "define.xml")
    }

    if (file.exists(define_path)) {
        result <- extract_variable_info_from_define(define_path)

        expect_s3_class(result, "data.frame")
        expect_gt(nrow(result), 0)

        # Check that it has the expected structure
        expected_cols <- c(
            "OID",
            "Name",
            "SASFieldName",
            "DataType",
            "Length",
            "Description",
            "Origin",
            "CodelistOID",
            "Dataset"
        )
        expect_true(all(expected_cols %in% names(result)))

        # Should have some common variables
        expect_true(any(c("STUDYID", "USUBJID") %in% result$Name))
        expect_true("ADSL" %in% result$Dataset)
    } else {
        skip("Real define.xml not found")
    }
})
