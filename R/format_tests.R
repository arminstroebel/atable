#' Formats hypothesis test results
#'
#' The results of function \code{\link{two_sample_htest}} and \code{\link{multi_sample_htest}}
#'  must be formated before printing. \code{format_tests} does this.
#'
#' This function defines which test results are printed in the final table and how they are formated.
#'
#' The format depends on the class \code{x}. See section methods.
#'
#' If you are not pleased with the current format you may alter these functions.
#' But you must keep the original output-format, see section Value.
#' Function \code{\link{check_format_tests}} checks if the output of \code{format_tests} is suitable for further processing.

#' @param x An object.
#' @param ... Passed from and to other methods.
#' @param format_tests.htest Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{format_tests.htest}.
#'   The function must mimic \code{\link{format_tests}}: arguments are \code{x} and the ellipsis ... .
#'   Result is a data.frame with 1 rows and unique colnames.
#' @param format_tests.htest_with_effect_size Analog to argument format_tests.htest
#'
#' @return
#' A non-empty data.frame with one row.
#' See also function \code{\link{check_format_tests}}.




#' @export
format_tests <- function(x, ...) {
    UseMethod("format_tests")
}

#' @export
#' @describeIn format_tests Defines how to format class \code{htest}.
#' Returns a data.frame with 1 rows. Column \code{p} contains the p-value of the \code{x}.

format_tests.htest <- function(x, format_tests.htest = NULL, ...) {

    if (is.function(format_tests.htest))
        return(format_tests.htest(x, ...))

    if (is.function(atable_options("format_tests.htest")))
        return(atable_options("format_tests.htest")(x, ...))


    p <- atable_options("format_p_values")(x$p.value)

    format_tests_out <- data.frame(p = p, stringsAsFactors = FALSE)

    return(format_tests_out)
}

#' @export
#' @describeIn format_tests Defines how to format class \code{htest_with_effect_size}.
#' Returns a data.frame with 1 rows. Column \code{p} contains the p-value of the \code{x}.
#' Column \code{stat} contains the teststatistic.
#' Column \code{Effect Size (CI)} contains a effect size and its 95\% Confidence interval.
format_tests.htest_with_effect_size <- function(x, format_tests.htest_with_effect_size = NULL,
    ...) {

    if (is.function(format_tests.htest_with_effect_size))
        return(format_tests.htest_with_effect_size(x, ...))

    if (is.function(atable_options("format_tests.htest_with_effect_size")))
        return(atable_options("format_tests.htest_with_effect_size")(x, ...))


    p <- atable_options("format_p_values")(x$p.value)


    effect_size <- atable_options("format_numbers")(x$effect_size)
    effect_size_CI_lower <- atable_options("format_numbers")(x$effect_size_CI_lower)
    effect_size_CI_upper <- atable_options("format_numbers")(x$effect_size_CI_upper)


    es_ci <- paste0(effect_size, " (", effect_size_CI_lower, "; ", effect_size_CI_upper,
        ")", sep = "")


    stat <- atable_options("format_numbers")(x$statistic)


    format_tests_out <- data.frame(p = p, stat = stat, Effect_Size = es_ci, stringsAsFactors = FALSE)

    format_tests_out <- doBy::renameCol(format_tests_out, "Effect_Size", "Effect Size (CI)")

    return(format_tests_out)

}


#' @export
#' @describeIn format_tests Tries to cast to data.frame with one row. Uses the names of the list as colnames.
format_tests.default <- function(x, ...) {


    y <- as.data.frame(x, stringsAsFactors = FALSE, make.names = FALSE)
    # this can go horribly wrong in several ways: -when x contains non-atomic objects
    # -atomic objects of different lengths -x has a lot of names, the table gets too
    # wide to print but better than nothing

    y[] <- lapply(y, atable_options("format_numbers"))

    return(y)
}
