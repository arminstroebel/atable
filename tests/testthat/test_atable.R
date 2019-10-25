context("atable")
library(atable)

library(plyr)

library(doBy)
# create data ####



DD = atable::test_data

n = nrow(DD)

DD$NA_Factor_with_levels = factor(rep(NA, n), levels = c("x","y"))
DD$NA_Factor_without_levels = droplevels(DD$NA_Factor_with_levels)
DD$NA_Numeric = as.numeric(rep(NA, n))
DD$NA_logical = as.logical(rep(NA, n))





DD = within(DD, {
  Group_non_valid_levels = mapvalues(Group, c("Treatment", "Control"), c(" 4jfu", ""))
  Group_non_valid_levels2 = mapvalues(Group, c("Treatment", "Control"), c(".76s", " "))
  Split1_non_valid_levels = mapvalues(Split1, from = levels(Split1), to=c(" 68 .", "7 j", ""))
  Split2_non_valid_levels = mapvalues(Split2, from = levels(Split2), to=c(" ", "split 2"))
  Factor_non_valid_levels = mapvalues(Factor, from = levels(Factor), to=c(" a."," ", "3", "asdf"))
})


DD_reload = DD

# create Data with missing values in group_col und split_col NA ####


set.seed(42)
n = 129
size = 3

the_labels_Factor = paste0("G", seq(from=size, to=0) ) # backwards
the_labels_Ordered = c("low", "medium", "high")

levels_split1 = c(LETTERS[2:1], NA)
levels_split2 = c(letters[2:1], NA)
levels_group_col = c("Treatment", "Control", NA)

nn = expand.grid(Split1 = levels_split1, Split2 = levels_split2, Group = levels_group_col)



NN = rbind(nn,nn,nn,nn,nn)
n=nrow(NN)
NN = within(NN, {
  Numeric = 3*rnorm(n) +4
  Numeric2 = 3*rnorm(n) +4
  Logical = base::sample(x=c(TRUE, FALSE, NA), size = n, replace=TRUE)
  Factor = factor(sample(the_labels_Factor, size=n, replace = TRUE, prob = c(2,2,1,1)) , labels = the_labels_Factor )
  Ordered = factor(sample(the_labels_Ordered, size=n, replace = TRUE, prob = c(1,1,1)) , labels = the_labels_Ordered, ordered = TRUE )})


NN$Split1_with_level_NA = addNA(NN$Split1)
NN$Split2_with_level_NA = addNA(NN$Split2)


# create empty data.frame ####

EE = DD[FALSE, ]



# apply atable with NA for split_cols, group_col and format_to (all possible combinations)  ####

a = list(
  x = DD,
  target_cols = c("Logical", "Numeric", "Factor", "Ordered", "Character"),
  group_col = "Group",
  split_cols = c("Split1","Split2"),
  format_to = "Latex")

# create a list with all possible argument combinations
setter = function(x, name, to){
  # sets a field 'name' of a list 'x' to 'to'
  # 'to' may be NULL, which does not remove the field
  x = if(is.null(to)){
    # cannot assign NULL to field of a list. Will remove that field
    # workaround:
    x[name] <- list(NULL)
    x
  }else{
    x[[name]] = to
    x}

  return(x)
}


the_arg_list = list(a)


the_arg_list = c(the_arg_list, list(setter(a, "split_cols", NULL)))
the_arg_list = c(the_arg_list, list(setter(a, "Group", NULL)))
the_arg_list = c(the_arg_list, list(setter(setter(a, "split_cols", NULL), "group_col", NULL)
) )




b = setter(a, "format_to", "Word")

the_arg_list = c(the_arg_list, list(b) )
the_arg_list = c(the_arg_list, list(setter(b, "split_cols", NULL)))
the_arg_list = c(the_arg_list, list(setter(b, "Group", NULL)))
the_arg_list = c(the_arg_list, list(setter(setter(b, "split_cols", NULL), "group_col", NULL)
) )


b = setter(a, "format_to", "Raw")

the_arg_list = c(the_arg_list, list(b) )
the_arg_list = c(the_arg_list, list(setter(b, "split_cols", NULL)))
the_arg_list = c(the_arg_list, list(setter(b, "Group", NULL)))
the_arg_list = c(the_arg_list, list(setter(setter(b, "split_cols", NULL), "group_col", NULL)
) )


# call atable on all arguments
b = lapply(the_arg_list, do.call, what="atable")

test_that("atable returns data.frame for format_to Latex and Word", {
  expect_true(all(sapply(b[1:8], is.data.frame)))
})

test_that("atable returns list for format_to Raw", {
  expect_true(all(sapply(b[9:12], is.list)))
})


test = b[[9]]$tests_result

# two sample htest applies the expected tests ####
test_that("two sample htest applies the expected tests", {
  expect_equal("Pearson's Chi-squared test with Yates' continuity correction", test$Logical[[1]]$method)

  expect_equal("Pearson's Chi-squared test", test$Factor[[1]]$method)

  expect_equal("Wilcoxon rank sum test with continuity correction", test$Ordered[[1]]$method)

  expect_equal("Two-sample Kolmogorov-Smirnov test", test$Numeric[[1]]$method)

})


# multi sample htest applies the expected tests ####



test_that("multi sample htest applies the expected tests", {

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group2"
  split_cols = c("Split1","Split2")
  format_to = "Raw"

  out = atable(test_data, target_cols, group_col, split_cols, format_to)


  expect_equal("Pearson's Chi-squared test", out$tests_result[1,"Logical"][[1]]$method )

  expect_equal("Pearson's Chi-squared test", out$tests_result[1,"Factor"][[1]]$method)

  expect_equal("Kruskal-Wallis rank sum test", out$tests_result[1,"Ordered"][[1]]$method)

  expect_equal("Kruskal-Wallis rank sum test", out$tests_result[1,"Numeric"][[1]]$method)

})
# just one of each column as target, nothing else ####
test_that("just one of each column as target, nothing else", {
  expect_true(is.data.frame(atable(DD, target_cols="Numeric", split_cols=NULL, group_col=NULL)))
  expect_true(is.data.frame(atable(DD, target_cols="Numeric", split_cols="Split1", group_col=NULL)))
  expect_true(is.data.frame(atable(DD, target_cols="Numeric", split_cols=NULL, group_col="Group")))
  expect_true(is.data.frame(atable(DD, target_cols="Numeric", split_cols="Split1", group_col="Group")))
})

# name clashes ####



test_that("target_cols has level atable_options('colname_for_variable')", {
  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("Split1","Split2")

  DD = within(DD, {Group = mapvalues(Group, from="Treatment", to=atable_options("colname_for_variable"))})

  expect_error(atable(DD, target_cols, group_col, split_cols))
})


test_that("target_cols in atable_options('colname_for_observations')", {

  DD = doBy::renameCol(DD, "Numeric", atable_options("colname_for_observations"))

  target_cols = c("Logical", atable_options("colname_for_observations"), "Factor", "Ordered")

  group_col = "Group"
  split_cols = c("Split1","Split2")


  expect_error(atable(DD, target_cols, group_col, split_cols))
})



test_that("group_col has level, also returned by format_tests(), 'p'", {


  DD = within(DD, {Group = mapvalues(Group, from = "Treatment", to = "p")})

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("Split1","Split2")


  expect_error(atable(DD, target_cols, group_col, split_cols))
})



test_that("splitcols has same name as result of format_tests() 'p'", {


  DD = doBy::renameCol(DD, "Split1", "p")

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("p","Split2")

  expect_error(atable(DD, target_cols, group_col, split_cols))
})


test_that("group_col has same level as split_cols'Split1'", {

  DD = within(DD, {Group = mapvalues(Group, from="Treatment", to="Split1")})

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("Split1","Split2")


  expect_error(atable(DD, target_cols, group_col, split_cols))
})


test_that("'tag' in split_cols", {

  DD = doBy::renameCol(DD, "Split1", "tag")

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("tag","Split2")

  expect_error(atable(DD, target_cols, group_col, split_cols))
})

test_that("'tag' in group_col", {

  DD = doBy::renameCol(DD, "Group", "tag")

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "tag"
  split_cols = c("Split1","Split2")

  expect_error(atable(DD, target_cols, group_col, split_cols))
})


test_that("'value' in split_cols", {

  DD = doBy::renameCol(DD, "Split1", "value")

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("value","Split2")

  expect_error(atable(DD, target_cols, group_col, split_cols))
})

test_that("'value' in group_col", {


  DD = doBy::renameCol(DD, "Group", "value")

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "value"
  split_cols = c("Split1","Split2")


  expect_error( atable(DD, target_cols, group_col, split_cols))

})

# formula method ####
test_that("formula method behaves as the data.frame method with suitable arguments", {



  ff = Logical + Numeric + Factor + Ordered ~ Group | Split1 + Split2

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("Split1","Split2")


  tab1 = atable(formula = ff, data = DD)
  tab2 = atable(DD, target_cols, group_col, split_cols)

  expect_true(all.equal(tab1, tab2) )




  ff = Logical + Numeric + Factor + Ordered ~ 1 | Split1 + Split2

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = NULL
  split_cols = c("Split1","Split2")

  tab1 = atable(formula = ff, data = DD)

  tab2 = atable(DD, target_cols, group_col, split_cols)

  expect_true(all.equal(tab1, tab2) )


  ff = Logical + Numeric + Factor + Ordered ~ Group

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = NULL

  tab1 = atable(formula = ff, data = DD)

  tab2 = atable(DD, target_cols, group_col, split_cols)

  expect_true(all.equal(tab1, tab2) )



  ff = Logical + Numeric + Factor + Ordered ~ 1

  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = NULL
  split_cols = NULL

  tab1 = atable(formula = ff, data = DD)

  tab2 = atable(DD, target_cols, group_col, split_cols)

  expect_true(all.equal(tab1, tab2) )


  ff = Logical + Numeric + Factor + Ordered ~ Group + Group2 | Split1 + Split2

  expect_error(atable(formula = ff, data = DD)) # two group_col
})


# empty data.frame ####
test_that("empty data.frame is allowed", {
  target_cols = c("Logical", "Numeric", "Factor", "Ordered")
  group_col = "Group"
  split_cols = c("Split1","Split2")

  expect_true(is.data.frame(atable(EE, target_cols, group_col, split_cols)))

  expect_true(is.data.frame(atable(EE, target_cols, group_col, split_cols, drop_levels = FALSE)))
})

# data.frame without names
test_that("data.frame without names not allowed", {
  expect_error(atable(unname(atable::test_data), target_cols = "Numeric"))
})

# data.frame with NA ####

test_that("data.frame with only NA is allowed", {
  target_cols = c("NA_logical")

  group_col = "NA_Numeric"
  split_cols = c("NA_Factor_without_levels")


  expect_true(is.data.frame(atable(DD, target_cols, group_col, split_cols)))

  expect_true(is.data.frame(atable(DD, target_cols, group_col, split_cols, drop_levels = TRUE)))

  atable_options(replace_NA_by = "replaced NA")
  expect_true(is.data.frame(atable(DD, target_cols, group_col, split_cols)))
  atable_options_reset()

})



# levels that are not syntactically valid ####

test_that("group_col has level '' (empty character)", {

  target_cols=c("Factor_non_valid_levels")
  group_col = "Group_non_valid_levels"
  split_cols = c("Split1_non_valid_levels", "Split2_non_valid_levels")


  the_table = atable(DD, target_cols, group_col, split_cols)
  # group_col has level ''. This level will be renamed to 'Var.4' or 'Var.5' etc depending on its position

  expect_false(all(levels(DD[[group_col]]) %in% colnames(the_table)))

})

test_that("group_col has level ' ' (blank)", {

  target_cols=c("Factor_non_valid_levels")
  group_col = "Group_non_valid_levels2"
  split_cols = c("Split1_non_valid_levels", "Split2_non_valid_levels")



  the_table = atable(DD, target_cols, group_col, split_cols) # ok, no renaming of levels


  expect_true(all(levels(DD[[group_col]]) %in% colnames(the_table)))
})


