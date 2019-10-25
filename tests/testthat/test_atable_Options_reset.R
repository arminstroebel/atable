context("atable_options_reset")
library(atable)




atable_options()
atable_options("replace_NA_by")
atable_options("replace_NA_by" = "asdf")


atable_options_reset()


test_that("option replacement is available", {
  expect_equal(atable_options("replace_NA_by"), "missing")
})
