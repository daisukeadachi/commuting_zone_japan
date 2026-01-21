# Simple test runner
if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat required to run tests")
cat('Running tests in tests/testthat...\n')
testthat::test_dir("tests/testthat")
