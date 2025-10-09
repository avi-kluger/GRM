test_that("run_grm works with basic input", {
  # Skip graphics tests in headless environments
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("ggmirt")
  
  # Create minimal test data
  data(Science, package = "mirt")
  
  # Test with plotting disabled to avoid graphics errors
  expect_no_error({
    results <- run_grm(Science[1:50, ], # Use subset for faster testing
                       save_plots = FALSE, 
                       display_plots = FALSE,
                       auto_display = FALSE)
  })
  
  # Basic structure tests
  expect_s3_class(results, "grm_analysis")
  expect_true("fit" %in% names(results))
  expect_true("reliability" %in% names(results))
})

test_that("helper functions work", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  
  # Test extract_item_labels
  test_data <- data.frame(
    item1 = c(1, 2, 3),
    item2 = c(2, 3, 1)
  )
  
  labels <- extract_item_labels(test_data)
  expect_equal(names(labels), c("item1", "item2"))
})