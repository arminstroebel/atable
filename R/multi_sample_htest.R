#' Calculates multi sample hypothesis tests
#'
#' Calculates multi sample hypothesis tests depending on the class of its input.
#'
#'
#' Calculates multi sample hypothesis tests depending on the class of its input.
#'
#' Results are passed to function \code{format_tests} for the final table.
#'
#' If you are not pleased with the current hypothesis tests you may alter these functions.
#' But you must keep the original output-format, see section Value.
#' Function \code{\link{check_tests}} checks if the output of statistics is suitable for further processing.
#'
#' The function \code{multi_sample_htest} is essentially a wrapper
#' to standardize the arguments of various hypothesis test functions.
#'
#'
#' @param value An atomic vector.
#' @param group A factor, same length as \code{value}.
#' @param ... Passed to methods.
#' @param multi_sample_htest.numeric Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::multi_sample_htest.numeric}.
#'   The function must mimic \code{\link{multi_sample_htest.numeric}}: arguments are
#'   \code{value}, \code{group} and the ellipsis ... .
#'   Result is a named list with \code{length} > 0 with unique names.
#' @param multi_sample_htest.ordered Analog to argument two_sample_htest.numeric
#' @param multi_sample_htest.factor Analog to argument two_sample_htest.numeric
#'
#' @return
#' A named list with length > 0.
#'
#' Most hypothesis-test-functions in R like \code{\link[stats]{t.test}} or \code{\link[stats]{chisq.test}} return an
#' object of class \code{'htest'}. \code{'htest'}-objects are a suitable output for function \code{two_sample_htest}.
#' Function \code{\link{check_tests}} checks if the output is suitable for further processing.


#' @export
multi_sample_htest <- function(value, group, ...) {
    UseMethod("multi_sample_htest")
}

#' @export
#' @describeIn multi_sample_htest Casts to factor and then calls method \code{multi_sample_htest} again.
multi_sample_htest.logical <- function(value, group, ...) {
    return(multi_sample_htest(as.factor(value), group, ...))
}

#' @export
#' @describeIn multi_sample_htest Calls \code{\link[stats]{chisq.test}}.
multi_sample_htest.factor <- function(value, group, multi_sample_htest.factor = NULL,
    ...) {

    if (is.function(multi_sample_htest.factor))
        return(multi_sample_htest.factor(value, group, ...))

    if (is.function(atable_options("multi_sample_htest.factor")))
        return(atable_options("multi_sample_htest.factor")(value, group, ...))


    test <- try(stats::chisq.test(group, value), silent = TRUE)

    out <- if (class(test) == "try-error") {
        warning("Not enough values. Returning p-value=NaN.")
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

    return(out)
}


#' @export
#' @describeIn multi_sample_htest Casts \code{value} to factor and then calls method \code{multi_sample_htest} again.
multi_sample_htest.character <- function(value, group, ...) {
    return(multi_sample_htest(as.factor(value), group, ...))
}


#' @export
#' @describeIn multi_sample_htest Calls \code{\link[stats]{kruskal.test}}.
multi_sample_htest.ordered <- function(value, group, multi_sample_htest.ordered = NULL,
    ...) {



    if (is.function(multi_sample_htest.ordered))
        return(multi_sample_htest.ordered(value, group, ...))

    if (is.function(atable_options("multi_sample_htest.ordered")))
        return(atable_options("multi_sample_htest.ordered")(value, group, ...))


    group <- factor(group)
    value <- as.numeric(value)  # kruskal.test demands class numeric, not ordered factor. Even when it is based on ranks

    test <- try(stats::kruskal.test(x = value, g = group, ...), silent = TRUE)

    out <- if (class(test) == "try-error") {
        warning("Not enough values. Returning NA.")
        test <- stats::kruskal.test(x = rep(c(1, 2), each = 4), g = factor(rep(letters[c(1,
            2)], each = 4)))
        test <- plyr::llply(test, function(x) NA)

        test$method <- "Kruskal-Wallis rank sum test"
        test$p.value <- NaN
        test$statistic <- NaN
        class(test) <- "htest"
        test

    } else {
        test
    }



    return(out)

}

#' @export
#' @describeIn multi_sample_htest Calls \code{multi_sample_htest}'s method on \code{ordered(value)}.
multi_sample_htest.numeric <- function(value, group, multi_sample_htest.numeric = NULL,
    ...) {

    if (is.function(multi_sample_htest.numeric))
        return(multi_sample_htest.numeric(value, group, ...))

    if (is.function(atable_options("multi_sample_htest.numeric")))
        return(atable_options("multi_sample_htest.numeric")(value, group, ...))


    return(multi_sample_htest(ordered(value), group, ...))
}
