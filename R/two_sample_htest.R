#' Two sample hypothesis tests and effect size
#'
#' Calculates two sample hypothesis tests and effect size depending on the class of its input.
#'
#'
#'
#' Results are passed to function \code{\link{format_tests}} for the final table.
#' So the results of \code{two_sample_htest} must have a class for which the generic
#' \code{\link{format_tests}} has a method.
#'
#' If you are not pleased with the current hypothesis tests you may alter these functions.
#' But you must keep the original output-format, see section Value.
#'
#' Note that the various statistical test functions in R have heterogeneous arguments:
#' for example \code{\link[stats]{chisq.test}}  and \code{\link[stats]{ks.test}} do not have
#' formula/data as arguments, whereas \code{\link[stats]{wilcox.test}} and
#' \code{\link[stats]{kruskal.test}} do. So the function \code{two_sample_htest} is essentially
#' a wrapper to standardize the arguments of various hypothesis test functions.
#'
#' As \code{two_sample_htest} is only intended to be applied to unpaired two sample data,
#' the two arguments \code{value} and \code{group} are sufficient to describe the data.
#'
#' Note that e.g. for class numeric the p-value is calculated by \code{ks.test} and the effects
#' size 95\% CI by \code{cohen.d}. As these are two different functions the results may be
#' contradicting: the p-value of \code{ks.test} can be smaller than 0.05
#' and the CI of \code{cohen.d} contains 0 at the same time.
#'
#'
#' @param value An atomic vector. These values will be tested.
#' @param group A factor with two levels and same length as \code{value}.
#' Defines the two groups of \code{value}, that are compared by a two sample hypothesis tests.
#'
#' @param ... Passed to methods.
#'
#'
#' @param two_sample_htest.numeric Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::two_sample_htest.numeric}.
#'   The function must mimic \code{\link{two_sample_htest.numeric}}: arguments are
#'   \code{value}, \code{group} and the ellipsis ... .
#'   Result is a named list with \code{length} > 0 with unique names.
#'
#' @param two_sample_htest.factor Analog to argument two_sample_htest.numeric
#' @param two_sample_htest.ordered Analog to argument two_sample_htest.numeric
#'
#'
#' @return
#' A named list with length > 0, where all elements of the list are atomic and have the same length.
#'
#' Most hypothesis-test-functions in R like \code{\link[stats]{t.test}} or \code{\link[stats]{chisq.test}}
#' return an object of class \code{'htest'}. \code{'htest'}-objects are a suitable output for function
#' \code{two_sample_htest}. Function \code{\link{check_tests}} checks if the output is suitable for
#' further processing.

#' @export
two_sample_htest <- function(value, group, ...) {
    UseMethod("two_sample_htest")
}

#' @export
#' @describeIn two_sample_htest Casts \code{value} to factor and then calls method \code{two_sample_htest} again.
two_sample_htest.character <- function(value, group, ...) {
    return(two_sample_htest(as.factor(value), group, ...))
}




#' @export
#' @describeIn two_sample_htest Calls \code{\link[stats]{chisq.test}} on \code{value}.
#' Effect size is the odds ratio calculated by \code{\link[stats]{fisher.test}} (if \code{value} has two levels),
#' or Cramer's V by \code{\link[DescTools]{CramerV}}.
two_sample_htest.factor <- function(value, group, two_sample_htest.factor = NULL,
    ...) {

    if (is.function(two_sample_htest.factor))
        return(two_sample_htest.factor(value, group, ...))

    if (is.function(atable_options("two_sample_htest.factor")))
        return(atable_options("two_sample_htest.factor")(value, group, ...))


    test <- try(stats::chisq.test(group, value), silent = TRUE)

    out <- if (class(test) == "try-error") {
        warning("Not enough values. Returning p-value=NaN..")
        test <- stats::chisq.test(x = matrix(10, nrow = 2, ncol = 2))  # some random data that allow a chisq.test without errors
        test <- plyr::llply(test, function(x) NA)

        test$method <- "Pearson's Chi-squared test"
        test$p.value <- NaN
        test$statistic <- NaN
        class(test) <- "htest"
        test
    } else {
        test
    }



    # calculate effect size with CI
    if (nlevels(value) == 2) {
        fisher_test <- stats::fisher.test(group, value)
        out$effect_size <- fisher_test$estimate
        out$effect_size_CI_lower <- fisher_test$conf.int[1]
        out$effect_size_CI_upper <- fisher_test$conf.int[2]
        out
    } else {



        # DescTools::CramersV also calls chisq.test.  chisq.test givees an error when
        # supllied empty data. I want NaN
        effect_size_test <- try(DescTools::CramerV(group, value, method = "fisheradj",
            conf.level = 0.95), silent = TRUE)


        effect_size <- if (class(effect_size_test) == "try-error") {
            warning("Not enough values. Returning effect size NaN.")
            c(NaN, NaN, NaN)
        } else {
            effect_size_test
        }


        out$effect_size <- effect_size[1]
        out$effect_size_CI_lower <- effect_size[2]
        out$effect_size_CI_upper <- effect_size[3]
        out
    }



    class(out) <- c("htest_with_effect_size", class(out))


    return(out)


}


#' @export
#' @describeIn two_sample_htest Casts \code{value} to factor and then calls \code{two_sample_htest} again.
two_sample_htest.logical <- function(value, group, ...) {
    return(two_sample_htest(as.factor(value), group, ...))
}

#' @export
#' @describeIn two_sample_htest Calls \code{\link[stats]{ks.test}} on \code{value}.
#'  Effect size is Cohen's d calculated by \code{\link[effsize]{cohen.d}}.
two_sample_htest.numeric <- function(value, group, two_sample_htest.numeric = NULL,
    ...) {


    if (is.function(two_sample_htest.numeric))
        return(two_sample_htest.numeric(value, group, ...))

    if (is.function(atable_options("two_sample_htest.numeric")))
        return(atable_options("two_sample_htest.numeric")(value, group, ...))


    d <- data.frame(value = value, group = group)

    group_levels <- levels(group)
    x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
    y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)


    test <- try(stats::ks.test(x, y, alternative = c("two.sided"), ...), silent = TRUE)

    out <- if (class(test) == "try-error") {
        warning("Not enough values. Returning p.value=NaN.")
        test <- stats::ks.test(c(1, 2), c(3, 4, 5), alternative = c("two.sided"))  # create valid data for ks.test
        test <- plyr::llply(test, function(x) NA)  # fill htest with NA
        test$method <- "Two-sample Kolmogorov-Smirnov test"
        test$p.value <- NaN
        test$statistic <- NaN
        class(test) <- "htest"
        test

    } else {
        test
    }



    # calculate effect size with CI
    effect_size_test <- try(effsize::cohen.d(d = value, f = group, na.rm = TRUE),
        silent = TRUE)


    effect_size <- if (class(effect_size_test) == "try-error") {
        warning("Not enough values. Returning effect size NaN.")
        list(estimate = NaN, conf.int = c(NaN, NaN))
    } else {
        effect_size_test
    }


    out$effect_size <- effect_size$estimate
    out$effect_size_CI_lower <- effect_size$conf.int[1]
    out$effect_size_CI_upper <- effect_size$conf.int[2]


    class(out) <- c("htest_with_effect_size", class(out))


    return(out)


}


#' @export
#' @describeIn two_sample_htest Calls \code{\link[stats]{wilcox.test}} on \code{value}.
#'  Effect size is Cliff's delta calculated by \code{\link[effsize]{cliff.delta}}.
two_sample_htest.ordered <- function(value, group, two_sample_htest.ordered = NULL,
    ...) {

    if (is.function(two_sample_htest.ordered))
        return(two_sample_htest.ordered(value, group, ...))

    if (is.function(atable_options("two_sample_htest.ordered")))
        return(atable_options("two_sample_htest.ordered")(value, group, ...))



    value <- as.numeric(value)  # wilcox.test demands class numeric, no ordered factor. Even when the test depends only on ranks
    data <- data.frame(value = value, group = group)

    test <- try(stats::wilcox.test(value ~ group, data, alternative = "two.sided",
        paired = FALSE), silent = TRUE)

    out <- if (class(test) == "try-error") {
        warning("Not enough values. Returning p.value NaN.")
        test <- stats::wilcox.test(x = c(1, 2), y = c(3, 4, 5), alternative = c("two.sided"),
            paired = FALSE)  # some test data that allow wilcox.test witout error
        test <- plyr::llply(test, function(x) NA)
        test$method <- "Wilcoxon rank sum test with continuity correction"
        test$p.value <- NaN
        test$statistic <- NaN
        class(test) <- "htest"
        test

    } else {
        test
    }



    effect_size_test <- try(effsize::cliff.delta(d = as.numeric(value), f = group),
        silent = TRUE)

    effect_size <- if (class(effect_size_test) == "try-error") {
        warning("Not enough values. Returning effect size NaN.")
        list(estimate = NaN, conf.int = c(NaN, NaN))
    } else {
        effect_size_test
    }

    out$effect_size <- effect_size$estimate
    out$effect_size_CI_lower <- effect_size$conf.int[1]
    out$effect_size_CI_upper <- effect_size$conf.int[2]


    class(out) <- c("htest_with_effect_size", class(out))

    return(out)
}
