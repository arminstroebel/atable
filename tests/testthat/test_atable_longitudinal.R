context("atable_longitudinal")
library(atable)



DD = atable::test_data


set.seed(42)
DD = within(DD,{time = sample(paste0("time_", 0:5), size=nrow(DD), replace = TRUE)})

split_cols = "time"
group_col = "Group"








test_that("call atable_longitudinal", {


  tab = atable_longitudinal(x = DD,
                            target_cols = "Split1",
                            group_col = group_col,
                            split_cols = split_cols,
                            add_margins = TRUE)



  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==8)
  expect_true(nrow(tab)==30)


  # no margin
  tab = atable_longitudinal(x = DD,
                            target_cols = "Split1",
                            group_col = group_col,
                            split_cols = split_cols,
                            add_margins = FALSE)



  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==7)
  expect_true(nrow(tab)==30)
})


test_that("target_cols with 2 levels", {


  tab = atable_longitudinal(x = DD,
                            target_cols = "Split2",
                            group_col = group_col,
                            split_cols = split_cols,
                            add_margins = TRUE)



  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==7)
  expect_true(nrow(tab)==6)

})


test_that("no group", {

  tab = atable_longitudinal(x = DD,
                            target_cols = "Numeric",
                            group_col = NULL,
                            split_cols = split_cols)



  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==2)
  expect_true(nrow(tab)==6)


  tab = atable_longitudinal(x = DD,
                            target_cols = "Split1",
                            group_col = NULL,
                            split_cols = split_cols)



  expect_true(is.data.frame(tab))
  expect_true(ncol(tab)==3)
  expect_true(nrow(tab)==30)

})



test_that("wrong arguments", {
  # more target_cols
  expect_error(
    atable_longitudinal(x = DD,
                        target_cols = c("Numeric", "Split1"),
                        group_col = group_col,
                        split_cols = split_cols)
  )


  # no split_cols
  expect_error(
    atable_longitudinal(x = DD,
                        target_cols = c("Numeric"),
                        group_col = group_col,
                        split_cols = NULL)
  )

  # more split_cols
  expect_error(
    atable_longitudinal(x = DD,
                        target_cols = c("Numeric"),
                        group_col = group_col,
                        split_cols = c("Split1", "Split2"))
  )
  # blocks given
  expect_error(
    atable_longitudinal(x = DD,
                        target_cols = c("Numeric"),
                        group_col = group_col,
                        split_cols = split_cols,
                        blocks = list("block"="Numeric"))
  )

})
