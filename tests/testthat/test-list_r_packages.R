test_that("list_r_packages returns correct structure for session packages", {
  result <- list_r_packages(use_renv = FALSE)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Package", "Version", "Title"))
  expect_true(nrow(result) > 0)
  expect_type(result$Package, "character")
  expect_type(result$Version, "character")
  expect_type(result$Title, "character")
})

test_that("list_r_packages handles renv with missing lockfile", {
  expect_error(
    list_r_packages(use_renv = TRUE, lockfile_loc = NULL),
    "lockfile_loc must be provided when use_renv = TRUE"
  )

  expect_error(
    list_r_packages(use_renv = TRUE, lockfile_loc = "nonexistent.lock"),
    "renv lockfile not found at: nonexistent.lock"
  )
})

test_that("get_package_info handles empty input", {
  result_empty_names <- get_package_info(pkg_names = character(0))
  result_null_names <- get_package_info(pkg_names = NULL)
  result_empty_renv <- get_package_info(renv_packages = list())

  expected_structure <- data.frame(
    Package = character(),
    Version = character(),
    Title = character(),
    stringsAsFactors = FALSE
  )

  expect_equal(result_empty_names, expected_structure)
  expect_equal(result_null_names, expected_structure)
  expect_equal(result_empty_renv, expected_structure)
})

test_that("get_package_info handles renv packages", {
  mock_renv_packages <- list(
    "testpkg1" = list(Version = "1.0.0", Title = "Test Package 1"),
    "testpkg2" = list(Version = "2.0.0", Title = "Test Package 2")
  )

  result <- get_package_info(renv_packages = mock_renv_packages)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$Package, c("testpkg1", "testpkg2"))
  expect_equal(result$Version, c("1.0.0", "2.0.0"))
  expect_equal(result$Title, c("Test Package 1", "Test Package 2"))
})

test_that("get_package_info handles missing renv package info", {
  mock_renv_packages <- list(
    "testpkg1" = list(Version = NULL, Title = NULL),
    "testpkg2" = list(Version = "2.0.0", Title = NULL)
  )

  result <- get_package_info(renv_packages = mock_renv_packages)

  expect_equal(result$Package, c("testpkg1", "testpkg2"))
  expect_equal(result$Version, c("Unknown", "2.0.0"))
  expect_equal(result$Title, c("No title available", "No title available"))
})

test_that("null coalescing operator works correctly", {
  expect_equal("test" %||% "default", "test")
  expect_equal(NULL %||% "default", "default")
  expect_equal(NA %||% "default", NA)
  expect_equal("" %||% "default", "")
})

test_that("with real renv lock.file", {
  test_path("scripts/renv.lock") -> lockfile_path

  result <- list_r_packages(use_renv = TRUE, lockfile_loc = lockfile_path)

  expect_snapshot_output(result)
})
