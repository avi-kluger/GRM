test_that("basic test", {
  expect_true(TRUE)
})

test_that("run_grm exists and is a function", {
  expect_true(exists("run_grm"))
  expect_type(run_grm, "closure")
})
