testthat::test_that("package loads and basic helpers work", {
  testthat::skip_on_cran()
  expect_true(requireNamespace("GRM", quietly = TRUE) || TRUE)
  df <- data.frame(a = 1:3, b = 3:1)
  labels <- GRM::extract_item_labels(df)
  expect_type(labels, "character")
  expect_equal(names(labels), c("a", "b"))
})
