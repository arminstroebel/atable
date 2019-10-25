context("replace_NA")
library(atable)


Character <- c(NA, letters[1:3], NA)
Factor <- factor(Character)
Ordered <- ordered(Factor)
Numeric <- rep(1, length(Factor))
Factor_without_NA <- factor(letters[1:length(Factor)])

DD <- data.frame(Character, Factor, Ordered, Numeric, Factor_without_NA, stringsAsFactors = FALSE)


test_that("does nothing", {
  expect_equal(replace_NA(Factor, replacement = c("asdf", "qwerty")), Factor) # replacement length 2
  expect_equal(replace_NA(DD, replacement = NULL), DD) # replacement length 0
  expect_equal(replace_NA(DD[c("Factor_without_NA", "Numeric")]), DD[c("Factor_without_NA", "Numeric")]) # no NA and class numeric
  expect_equal(replace_NA(Factor_without_NA), Factor_without_NA) # no NA

})

test_that("no NA but new level", {
  expect_equal(any(is.na(replace_NA(Factor))), FALSE)
  expect_equal(any(is.na(replace_NA(Character))), FALSE)
  expect_true("new NA" %in% levels(replace_NA(Factor, replacement = "new NA")))
  expect_warning(replace_NA(Factor, replacement = "a"))
})



test_that("class should not change", {
  expect_true(is.data.frame(replace_NA(DD)))
  expect_equal(lapply(replace_NA(DD), class), lapply(DD, class))
})




new_replacement = "replaced by test_options.R"
atable_options(replace_NA_by = new_replacement)

x <- c(NA, letters[1:3], NA)
y = replace_NA(x)

test_that("replace_NA takes replacement from atable_options", {
  expect_equal(sum(y==atable_options("replace_NA_by")), 2)
  expect_equal(sum(y==new_replacement), 2)
})

