context("atable_compact")
library(atable)




# Split2 has two levels. So only its first level is displayed.



test_that("call atable_compact", {


  tab = atable_compact(atable::test_data,
                       target_cols = c("Numeric", "Factor", "Split2"))

  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==2)
  expect_true(nrow(tab)==9)

  # only first level displayed as Split2 has only two levels, as defined by format_statistics_compact.statistics_factor of atable_options
  expect_true(levels(atable::test_data[["Split2"]])[1] == tab[9,1])

})

test_that("grouped", {
  tab = atable_compact(atable::test_data,
                       target_cols = c("Numeric", "Factor", "Split2"),
                       group_col = "Group")

  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==6)
  expect_true(nrow(tab)==9)
})

test_that("grouped margins", {
  tab = atable_compact(atable::test_data,
                       target_cols = c("Numeric", "Factor", "Split2"),
                       group_col = "Group",
                       add_margins=TRUE)

  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==7)
  expect_true(nrow(tab)==9)
})

test_that("grouped margin blocks", {

  tab = atable_compact(atable::test_data,
                       target_cols = c("Numeric", "Factor", "Split2", "Split1"),
                       group_col = "Group",
                       add_margins=TRUE,
                       blocks = list("Primary" = "Numeric",
                                     "Secondary" = c("Factor", "Split2"))
  )

  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==7)
  expect_true(nrow(tab)==16)

  expect_true(all(c("Primary", "Secondary") %in% tab[[1]]))
})

test_that("alias", {

  DD = atable::test_data

  attr(DD$Numeric, 'alias') = 'Consumption [Miles (US)/ gallon]'
  Hmisc::label(DD$Split1) = 'A Splitting Variable'
  units(DD$Split1) = 's'


  tab = atable_compact(DD,
                       target_cols = c("Numeric", "Factor", "Split2", "Split1")  )

  expect_true(all(c("Consumption [Miles (US)/ gallon]", "A Splitting Variable [s]") %in% tab[[1]]))

})

test_that("alias blocks", {

  DD = atable::test_data

  attr(DD$Numeric, 'alias') = 'Consumption [Miles (US)/ gallon]'
  Hmisc::label(DD$Split1) = 'A Splitting Variable'
  units(DD$Split1) = 's'


  tab = atable_compact(DD,
                       target_cols = c("Numeric", "Factor", "Split2", "Split1"),
                       blocks = list("First Block" = c("Numeric", "Factor"),
                                     "Second Block" = "Split1"))

  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==2)
  expect_true(nrow(tab)==16)

  expect_true("First Block" == tab[[2,1]])
  expect_true("Second Block" == tab[[11,1]])

  expect_true(grepl(pattern = "Consumption [Miles (US)/ gallon]", x = tab[[3,1]], fixed = TRUE))
  expect_true(grepl(pattern = "A Splitting Variable [s]", x = tab[[12,1]], fixed = TRUE))

})
