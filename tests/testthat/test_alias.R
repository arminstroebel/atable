context("alias")
library(atable)
library(Hmisc)


# Test Data ####

# Create random data with attributes containing aliases.
n = 40 * 3 * 3

DD = data.frame(alias_in_attr = rnorm(n),
                no_alias_ = rnorm(n),
                hmisc_labelled_without_units = rnorm(n),
                hmisc_labelled_with_units = rnorm(n),
                Group_no_alias = sample(c("a", "b", "c"), size=n, replace=TRUE),
                Group_alias = sample(c("A", "B", "C"), size=n, replace=TRUE),
                Split_no_alias = sample(c("d", "e", "f"), size=n, replace=TRUE),
                Split_alias = sample(c("D", "E", "F"), size=n, replace=TRUE),
                alias_in_different_attr = rnorm(n))


add_na = function(x){
  i = sample(1:n, size=round(n/20))
  x[i]=NA
  return(x)
}

DD[] = lapply(DD, add_na)

# I add exclamations marks to the aliases, so that I can distinguish them from the (syntactically valid) column names.
attr(DD$alias_in_attr, "alias") <- "!alias! [mmHg] alias_in_attr"

attr(DD$alias_in_different_attr, "alias_in_different_attr") <- "!alias! alias_in_different_attr"

attr(DD$Split_alias, "alias") <- "!alias! Split_alias"
attr(DD$Group_alias, "alias") <- "!alias! Group_alias"

Hmisc::label(DD$hmisc_labelled_without_units) <- "!label! hmisc labelled without units"

Hmisc::label(DD$hmisc_labelled_with_units) <- "!label! hmisc labelled with units"

# Hmisc::units(DD$hmisc_labelled_with_units) <- "unit hmisc labelled with units" # Hmisc::units(x)<- is not exported...
units(DD$hmisc_labelled_with_units) <- "!unit! hmisc labelled with units" # works



# Test atable ####
test_that("atable has the right aliases in the output", {

  # I only test with split and group, because aliases are independent of split and group, see function arrange_helper.
  tab = atable(alias_in_attr + no_alias_+ hmisc_labelled_without_units + hmisc_labelled_with_units ~ Group_alias | Split_no_alias + Split_alias, DD, format_to = "Console")

  expect_true(any(grepl(x = tab$Group, pattern = "!alias! [mmHg] alias_in_attr"), fixed = TRUE))

  expect_true(any(grepl(x = tab$Group, pattern = "!alias! Split_alias"), fixed = TRUE))

  expect_true(any(grepl(x = tab$Group, pattern = "!alias! Group_alias"), fixed = TRUE))

  expect_true(any(grepl(x = tab$Group, pattern = "!label! hmisc labelled without units"), fixed = TRUE))

  expect_true(any(grepl(x = tab$Group, pattern = "!label! hmisc labelled with units [!unit! hmisc labelled with units]"), fixed = TRUE))

})

# test create_alias_mapping ####

test_that("create_alias_mapping", {

  The_Alias_mapping = create_alias_mapping(DD)

  expect_equal(subset(The_Alias_mapping, old == "alias_in_attr", select ="new", drop = TRUE),
               "!alias! [mmHg] alias_in_attr")

  expect_equal(subset(The_Alias_mapping, old == "Split_alias", select ="new", drop = TRUE),
               "!alias! Split_alias")

  expect_equal(subset(The_Alias_mapping, old == "Group_alias", select ="new", drop = TRUE),
               "!alias! Group_alias")

  expect_equal(subset(The_Alias_mapping, old == "hmisc_labelled_without_units", select ="new", drop = TRUE),
               "!label! hmisc labelled without units")

  expect_equal(subset(The_Alias_mapping, old == "hmisc_labelled_with_units", select ="new", drop = TRUE),
               "!label! hmisc labelled with units [!unit! hmisc labelled with units]")

  # alias in different attribute. Hence atable_options("modifiy_colnames_without_alias") should be called:
  expect_equal(subset(The_Alias_mapping, old == "alias_in_different_attr", select ="new", drop = TRUE),
               atable_options("modifiy_colnames_without_alias")("alias_in_different_attr"))

})


# replace get_alias.default ####
test_that("replace get_alias.default via atable_options", {

  # change attributes for alias:
  atable_options('get_alias.default' = function(x, ...){attr(x, "alias_in_different_attr", exact = TRUE)})

  A = create_alias_mapping(DD)



  expect_equal(subset(A, old=="alias_in_different_attr", select = "new", drop = TRUE), "!alias! alias_in_different_attr")

  tab = atable(DD, "alias_in_different_attr", format_to = "Console")

  # the alias must be somewhere in the atable-results
  expect_true(any(grepl(pattern = "!alias! alias_in_different_attr",
                        x = tab$Group,
                        fixed = TRUE)))

  atable_options_reset() # reset the defautl attribute for alias

})

# replace get_alias.labelled ####
test_that("replace get_alias.labelled via atable_options", {

  # get_alias.labelled should use () for units and the label:
  atable_options('get_alias.labelled' = function(x, ...){
    out = attr(x, "label", exact = TRUE)

    Units = attr(x, "units", exact = TRUE)

    out = paste0("(",out, ")")

    out = if(!is.null(Units)){
      paste0(out, " (", Units, ")")
    }else{out}

    return(out)
  })

  A = create_alias_mapping(DD)



  expect_equal(subset(A, old=="hmisc_labelled_with_units", select = "new", drop = TRUE),
               "(!label! hmisc labelled with units) (!unit! hmisc labelled with units)")

  expect_equal(subset(A, old=="hmisc_labelled_without_units", select = "new", drop = TRUE),
               "(!label! hmisc labelled without units)")

  atable_options_reset() # reset the defautl attribute for alias

})
