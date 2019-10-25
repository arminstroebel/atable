context("indent_data_frame")
library(atable)


library(plyr)
library(doBy)
DD = expand.grid(Arm = paste0("Arm ",c(1,2,3)),
                 Gender = c("Male", "Female"), Haircolor = c("Red", "Green", "Blue"),
                 Income = c("Low", "Med", "High"), stringsAsFactors=TRUE )

DD = doBy::orderBy(~ Arm+Gender+Haircolor+Income, DD)

DD$values1 = runif(dim(DD)[1])
DD$values2 = 1
DD$values3 = sample(letters[1:4], size=nrow(DD), replace = TRUE)


keys = c("Arm", "Gender", "Haircolor", "Income")
values = c("values1", "values2", "values3")

character_empty = ""
numeric_empty = NA
indent_character = "\\quad"
indent_character = "  "
colname_indent = "Group"


DD_indent = indent_data_frame(DD, keys, values, character_empty, numeric_empty,  indent_character, colname_indent )


# random row order
set.seed(42)
DD_reordered = DD[sample(1:nrow(DD), nrow(DD)), ]

DD_reordered_indent = indent_data_frame(DD_reordered, keys, values, character_empty, numeric_empty,  indent_character, colname_indent )



test_that("colnames", {
  expect_equal(colnames(DD_indent), c(colname_indent, values) )
})


test_that("number of rows", {
  N_levels = sapply(DD[keys], function(x)nlevels(x))
  expect_equal(nrow(DD_indent), sum(cumprod(N_levels)))
})


test_that("reordered data.frame has same indent as ordered", {
  expect_equal(DD_indent, DD_reordered_indent)
})
