context("blocks")
library(atable)


# test data

mtcars = datasets::mtcars


attr(mtcars$mpg, "alias") <- "Consumption [Miles (US)/ gallon]"

Hmisc::label(mtcars$qsec) <- "Quarter Mile Time"
units(mtcars$qsec) = "s"

attr(mtcars$disp, "alias") <- "Displacement (cu.in.)"
attr(mtcars$am, "alias") <- "Transmission (0 = automatic, 1 = manual)"

attr(mtcars$cyl, "alias") <- "cylinder (#)"

a <- atable::atable(mpg + qsec + disp + hp + drat + am~ cyl,
                   mtcars,
                   # format_to = "Console",
                   blocks = list("Block" = c("mpg", "qsec"),
                                 "Another Block" = "drat" ),
                   format_to = "latex")

test_that("blocking works as intended", {

  expect_true( any(grepl(x = a$Group, pattern = "Block", fixed = FALSE)) )
  expect_true( any(grepl(x = a$Group, pattern = "Another Block", fixed = FALSE)) )
  expect_equal(nrow(a), 22)
  })

test_that("error checks for blocks", {

# error when split_cols and blocks are both not NULL
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl | carb,
                               mtcars,
                               blocks = list("Block" = c("mpg", "qsec"),
                                             "Another Block" = "drat" ) ) )


  # block not in target_cols
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl,
                               mtcars,
                               blocks = list("Block" = c("not in target_cols", "qsec"),
                                             "Another Block" = "drat" ) ) )

  # names in a block are not consecutive
  # between "mpg" and "disp" is "qsec"
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl,
                               mtcars,
                               blocks = list("Block" = c("mpg", "disp"),
                                             "Another Block" = "drat" ) ) )


  # names in block in wrong order
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl,
                               mtcars,
                               blocks = list("Block" = c("mpg", "disp", "qsec"),
                                             "Another Block" = "drat" ) ) )

  # blocks without names
  blocks <-list(c("mpg", "qsec"), "drat")
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl,
                               mtcars,
                               blocks = blocks ) )

  # NA in names(block)
  names(blocks) = c("block", NA)
  expect_error( atable::atable(mpg + qsec + disp + hp + drat + am ~ cyl,
                               mtcars,
                               blocks = blocks ) )

})

