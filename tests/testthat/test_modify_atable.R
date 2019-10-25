context("modify_atable")
library(atable)




DD <- atable::test_data

test_that("statistics applies modified function", {

  # statistics ####

  # class numeric ####
  # unmodified
  tab = atable(DD, target_cols = "Numeric")
  expect_equal(tab$value[5], "117 (12)")

  # local
  new_value_numeric = "local numeric"
  statistics_numeric_local  = function(x, ...){list(stats_numeric = 1,
                                                    local_call = new_value_numeric)}
  tab = atable(atable::test_data, target_cols = "Numeric",
               statistics.numeric = statistics_numeric_local )
  expect_equal(tab$value[5], new_value_numeric)

  # global option
  statistics_numeric_global_option = function(x, ...){list(atable_options_numeric = 1,
                                                           option = "global option numeric")}
  atable_options('statistics.numeric' = statistics_numeric_global_option)


  tab = atable(atable::test_data, target_cols = "Numeric" )
  expect_equal(tab$value[5], "global option numeric")


  # local overwrites global option
  tab = atable(atable::test_data, target_cols = "Numeric",
               statistics.numeric = statistics_numeric_local )
  expect_equal(tab$value[5], new_value_numeric)
  expect_equal(class(atable_options("statistics.numeric")), "function")

  # return to default
  atable_options_reset()
  tab = atable(DD, target_cols = "Numeric")
  expect_equal(tab$value[5], "117 (12)")


  # class numeric splitted and/or grouped ####

  # splitted
  tab = atable(atable::test_data, target_cols = "Numeric", split_cols = "Split1",
               statistics.numeric = statistics_numeric_local)
  expect_equal(tab$value[18], new_value_numeric)

  # grouped
  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group",
               statistics.numeric = statistics_numeric_local)

  expect_equal(tab$Control[5], new_value_numeric)


  # splitted and grouped
  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group", split_cols = "Split1",
               statistics.numeric = statistics_numeric_local)

  expect_equal(tab$Treatment[18], new_value_numeric)
  expect_equal(tab$Control[12], new_value_numeric)

  # class factor ####
  # local factor
  statistics_factor_local  = function(x, ...){list(stats_Factor = 1,
                                                   local_call = "local factor")}
  tab = atable(atable::test_data, target_cols = "Factor", statistics.factor = statistics_factor_local )
  expect_equal(tab$value[5], "local factor")


  # global factor
  statistics_factor_global_option = function(x, ...){list(atable_options_factor = 1,
                                                          option = "global option factor")}
  atable_options('statistics.factor' = statistics_factor_global_option)

  tab = atable(atable::test_data, target_cols = "Factor" )
  atable_options_reset()
  expect_equal(tab$value[5], "global option factor")




  # class ordered ####
  # local ordered
  statistics_ordered_local  = function(x, ...){list(stats_ordered = 1,
                                                    local_call = "local ordered")}
  tab = atable(atable::test_data, target_cols = "Ordered", statistics.ordered = statistics_ordered_local )
  expect_equal(tab$value[5], "local ordered")


  # global ordered
  statistics_ordered_global_option = function(x, ...){list(atable_options_ordered = 1,
                                                           option = "global option ordered")}
  atable_options('statistics.ordered' = statistics_ordered_global_option)

  tab = atable(atable::test_data, target_cols = "Ordered" )
  atable_options_reset()
  expect_equal(tab$value[5], "global option ordered")



  # two_sample_htest ####

  # class factor ####

  # two_sample_htest factor local
  two_sample_htest_factor_local = function(value, group, ...){
    list(two_sample_htest_factor_local = "1",
         local = "two sample htest factor local") }

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group",
               two_sample_htest.factor  = two_sample_htest_factor_local)

  expect_equal(tab$local[4], "two sample htest factor local")


  # two_sample_htest factor global
  two_sample_htest_factor_global = function(value, group, ...){
    list(two_sample_htest_factor_global = "2",
         global = "two sample htest factor global") }

  atable_options('two_sample_htest.factor' = two_sample_htest_factor_global)

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group")


  expect_equal(tab$global[4], "two sample htest factor global")


  # grouped splitted global
  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group", split_cols = "Split2")

  atable_options_reset()
  expect_equal(tab$global[14], "two sample htest factor global")

  # class numeric ####
  #  two_sample_htest numeric local
  # Example from the article. Test if arguments value and group are also passed and processed as expected.

  new_two_sample_htest_numeric <- function(value, group, ...){

    d <- data.frame(value = value, group = group)

    group_levels <- levels(group)
    x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
    y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)

    ks_test_out <- stats::ks.test(x, y)
    t_test_out <- stats::t.test(x, y)

    # return p-values of both tests
    out <- list(p_ks = ks_test_out$p.value,
                p_t = t_test_out$p.value )

    return(out)
  }


  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group",
               two_sample_htest.numeric = new_two_sample_htest_numeric)


  expect_equal(colnames(tab)[4], "p\\_ks")
  expect_equal(tab[4, "p\\_ks"], "0.36")


  # class ordered ####

  # two_sample_htest ordered local
  two_sample_htest_ordered_local = function(value, group, ...)
  {list(two_sample_htest_ordered_local = "1",
        local = "two sample htest ordered local") }

  tab = atable(atable::test_data, target_cols = "Ordered", group_col = "Group",
               two_sample_htest.ordered  = two_sample_htest_ordered_local)

  expect_equal(tab$local[4], "two sample htest ordered local")


  # two_sample_htest ordered global
  two_sample_htest_ordered_global = function(value, group, ...){
    list(two_sample_htest_ordered_global = "2",
         local = "two sample htest ordered global") }

  atable_options('two_sample_htest.ordered' = two_sample_htest_ordered_global)

  tab = atable(atable::test_data, target_cols = "Ordered", group_col = "Group")
  atable_options_reset()

  expect_equal(tab$local[4], "two sample htest ordered global")


  # multi_sample_htest ####
  # class numeric
  # local

  multi_sample_htest_numeric_local = function(value, group, ...)
  {list(multi_sample_htest_numeric_local = "1",
        local = "multi sample htest numeric local") }


  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group2",
               multi_sample_htest.numeric = multi_sample_htest_numeric_local)

  expect_equal(tab$local[4], "multi sample htest numeric local")

  # global
  multi_sample_htest_numeric_global = function(value, group, ...)
  {list(multi_sample_htest_numeric_global = "1",
        global = "multi sample htest numeric global") }


  atable_options('multi_sample_htest.ordered' = multi_sample_htest_numeric_global)

  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group2")
  atable_options_reset()

  expect_equal(tab$global[4], "multi sample htest numeric global")


  # class factor

  # local

  multi_sample_htest_factor_local = function(value, group, ...)
  {list(multi_sample_htest_numeric_local = "1",
        local = "multi sample htest factor local") }


  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2",
               multi_sample_htest.factor = multi_sample_htest_factor_local)

  expect_equal(tab$local[4], "multi sample htest factor local")

  # global
  multi_sample_htest_factor_global = function(value, group, ...)
  {list(multi_sample_htest_factor_global = "1",
        global = "multi sample htest factor global") }


  atable_options('multi_sample_htest.factor' = multi_sample_htest_factor_global)

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2")
  atable_options_reset()

  expect_equal(tab$global[4], "multi sample htest factor global")


  # class ordered

  # local

  multi_sample_htest_ordered_local = function(value, group, ...)
  {list(multi_sample_htest_ordered_local = "1",
        local = "multi sample htest ordered local") }


  tab = atable(atable::test_data, target_cols = "Ordered", group_col = "Group2",
               multi_sample_htest.ordered = multi_sample_htest_ordered_local)

  expect_equal(tab$local[4], "multi sample htest ordered local")

  # global
  multi_sample_htest_ordered_global = function(value, group, ...)
  {list(multi_sample_htest_ordered_global = "1",
        global = "multi sample htest ordered global") }


  atable_options('multi_sample_htest.ordered' = multi_sample_htest_ordered_global)

  tab = atable(atable::test_data, target_cols = "Ordered", group_col = "Group2")
  atable_options_reset()

  expect_equal(tab$global[4], "multi sample htest ordered global")


  # format_statistics ####
  # statistics_numeric ####

  # function statistics.numeric returns mean and sd

  format_statistics_numeric_local = function(x, ...){

    SD_Mean = paste(round(c(x$sd, x$mean), digits = 1), collapse = "; ")

    out <- data.frame(
      tag = factor("SD; Mean local"),
      value = paste0(SD_Mean, " local"),
      stringsAsFactors = FALSE)

    return(out)
  }

  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group2",
               format_statistics.statistics_numeric = format_statistics_numeric_local)

  expect_equal(tab$Group1[4], "1; 0.2 local")



  format_statistics_numeric_global = function(x, ...){

    SD_Mean = paste(round(c(x$sd, x$mean), digits = 1), collapse = "; ")

    out <- data.frame(
      tag = factor("SD; Mean global"),
      value = paste0(SD_Mean, " global"),
      stringsAsFactors = FALSE)

    return(out)
  }

  atable_options(format_statistics.statistics_numeric = format_statistics_numeric_global)
  tab = atable(atable::test_data, target_cols = "Numeric", group_col = "Group2")
  atable_options_reset()

  expect_equal(tab$Group1[4], "1; 0.2 global")


  # statistics_factor ####
  format_statistics_factor_local = function(x, ...){
    out <- data.frame(
      tag = factor("format factor local"),
      value = "format factor local",
      stringsAsFactors = FALSE)

    return(out)
  }

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2",
               format_statistics.statistics_factor = format_statistics_factor_local)

  expect_equal(tab$Group1[4], "format factor local")


  format_statistics_factor_global = function(x, ...){
    out <- data.frame(
      tag = factor("format factor global"),
      value = "format factor global",
      stringsAsFactors = FALSE)

    return(out)
  }

  atable_options(format_statistics.statistics_factor = format_statistics_factor_global)
  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2")
  atable_options_reset()

  expect_equal(tab$Group1[4], "format factor global")

  # format_tests ####
  # multi_sample_htest() returns an object of class htest. two_sample_htest returns htest_with_effect_size

  # class htest ####

  format_tests_htest_local = function(x, ...){
    return(data.frame(local ="format htest local"))
  }

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2", split_cols = "Split1",
               format_tests.htest = format_tests_htest_local)

  expect_equal(tab$local[23], "format htest local")


  format_tests_htest_global = function(x, ...){
    return(data.frame(local ="format htest global"))
  }

  atable_options(format_tests.htest = format_tests_htest_global)

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group2", split_cols = "Split1")

  expect_equal(tab$local[23], "format htest global")
  atable_options_reset()


  # class htest_with_effect_size ####


  format_tests_htest_with_effect_size_local = function(x, ...){
    return(data.frame(local ="format htest effsize local"))
  }

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group", split_cols = "Split1",
               format_tests.htest_with_effect_size = format_tests_htest_with_effect_size_local)

  expect_equal(tab$local[23], "format htest effsize local")



  format_tests_htest_with_effect_size_global = function(x, ...){
    return(data.frame(local ="format htest effsize global"))
  }

  atable_options(format_tests.htest_with_effect_size = format_tests_htest_with_effect_size_global)

  tab = atable(atable::test_data, target_cols = "Factor", group_col = "Group", split_cols = "Split1")

  expect_equal(tab$local[23], "format htest effsize global")

  atable_options_reset()
})
