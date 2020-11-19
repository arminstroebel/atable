context("atable_compact")
library(atable)


DD = atable::test_data

target_cols = c("Numeric", "Factor", "Split2") # Split2 has tow levels. So
a = atable_compact(DD,
                   target_cols = target_cols,
                   group_col = "Group",
                   split_cols = NULL,
                   format_to = "Console"
)


test_that("call atable_compact", {
  # only first level displayed as Split2 has only two levels, as defined by format_statistics_compact.statistics_factor of atable_options
  expect_true(subset(a, variable___=="Split2", "tag", drop=TRUE) == levels(DD$Split2)[1])

  # 'Mean_SD' is hardcoded in function format_statistics_compact.statistics_numeric in atable_options
  expect_true(subset(a, variable___=="Numeric", "tag", drop=TRUE) == "Mean_SD")

  expect_equal(nrow(a), length(target_cols) + 1) # +1 for observation row

})


test_that("atable with indent = FALSE gives similar format as atable_compact", {

  a = atable(DD,
             indent = FALSE,
             target_cols = target_cols,
             format_to = "Console")

  expect_true(all(class(a) %in% c("atable", "data.frame")))
})



Hmisc::label(DD$Numeric) ="A Number"
units(DD$Numeric) = "Arbitrary units"

attr(DD$Factor, "alias") = "An Alias"

test_that("add_margins and atable_compact", {

  a = atable_compact(DD,
                     target_cols = target_cols,
                     group_col = "Group",
                     split_cols = NULL,
                     add_margins = TRUE,
                     format_to = "Console")

  expect_true("A Number [Arbitrary units]" == a$variable___[2] )
  expect_true("An Alias" == a$variable___[3] )
  expect_true(all(colnames(a) == c("variable___",  "tag", "Total", "Treatment", "Control", "p", "stat", "Effect Size (CI)")))

  expect_true(nrow(a) == length(target_cols) + 1) # +1 for observation row
})


