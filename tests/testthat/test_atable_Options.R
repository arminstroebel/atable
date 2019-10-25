context("options")
library(atable)




test_that("option replace_NA_by is available", {
  expect_equal(atable_options("replace_NA_by"), "missing")
})
