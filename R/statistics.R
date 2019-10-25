#' Calculates descriptive statistics
#'
#' Calculates descriptive statistics depending on the class of its input.
#'
#'
#' Calculates descriptive statistics depending on the class of its input.
#'
#' Results are passed to function \code{\link{format_statistics}}.
#'
#' If you are not pleased with the current descriptive statistics you may alter these functions.
#' But you must keep the original output-format, see section Value.
#' Function \code{\link{check_statistics}} checks if the output of statistics is suitable for further processing.
#'
#'
#' @param x An object. Statistics will be calculated of \code{x}.
#' @param ... Passed from and to other methods.
#' @param labels_TRUE_FALSE For relabeling logicals. See also \code{\link{atable_options}}.
#' @param statistics.numeric Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::statistics.numeric}.
#'   The function must mimic \code{\link{statistics}}: arguments are \code{x} and the ellipsis ... .
#'   Result is a named list with \code{length} > 0 with unique names.
#'
#' @param statistics.factor Analog to argument statistics.numeric
#' @param statistics.ordered Analog to argument statistics.numeric
#'
#' @return
#' The results of \code{statistics} are passed to function \code{\link{format_statistics}}.
#' So the results of \code{statistics} must have a class for which the generic \code{\link{format_statistics}} has a method.
#'
#' \code{\link{format_statistics}} has a default method, which accepts lists. So the results of \code{statistics} can be a
#' named list with length > 0. The names of the list must have no duplicates.
#'
#' Function \code{\link{check_statistics}} checks if the output of statistics is suitable for further processing.
#'


#' @export
statistics <- function(x, ...) {
    UseMethod("statistics")
}


#' @export
#' @describeIn statistics Descriptive statistics are: length, number of missing values, mean and standard deviation.
#' Class of the result is \code{'statistics_numeric'} and there is a method \code{format_statistics_to_Latex.statistics_numeric}.
#' This function is meant for interval scaled variables.
statistics.numeric <- function(x, statistics.numeric = NULL, ...) {


    if (is.function(statistics.numeric))
        return(statistics.numeric(x, ...))

    if (is.function(atable_options("statistics.numeric")))
        return(atable_options("statistics.numeric")(x, ...))

    na.rm <- TRUE

    statistics_out <- list(length = length(x), missing = sum(is.na(x)), mean = mean(x,
        na.rm = na.rm), sd = stats::sd(x, na.rm = na.rm))

    class(statistics_out) <- c("statistics_numeric", "list")
    return(statistics_out)
}

#' @export
#' @describeIn statistics Counts the numbers of occurrences of the levels of \code{x}
#' with function \code{\link[base]{table}}.
#' This function is meant for nominal and ordinal scaled variables.
statistics.factor <- function(x, statistics.factor = NULL, ...) {

    if (is.function(statistics.factor))
        return(statistics.factor(x, ...))

    if (is.function(atable_options("statistics.factor")))
        return(atable_options("statistics.factor")(x, ...))

    statistics_out <- table(x, useNA = "always")

    statistics_out <- as.list(statistics_out)

    class(statistics_out) <- c("statistics_factor", "list")
    return(statistics_out)

}

#' @export
#' @describeIn statistics Casts \code{x} to factor, then applies \code{statistics} again. The labels for \code{TRUE} and \code{FALSE} can also be modfied by setting \code{atable_options('labels_TRUE_FALSE')}.
statistics.logical <- function(x, labels_TRUE_FALSE = atable_options("labels_TRUE_FALSE"),
    ...) {
    x <- factor(x, levels = c(TRUE, FALSE), labels = labels_TRUE_FALSE)

    return(statistics(x, ...))
}

#' @export
#' @describeIn statistics Casts \code{x} to factor, then applies \code{statistics} again.
statistics.character <- function(x, ...) {
    return(statistics(as.factor(x), ...))
}

#' @export
#' @describeIn statistics Casts \code{x} to factor, then applies \code{statistics} again.
statistics.ordered <- function(x, statistics.ordered = NULL, ...) {

    if (is.function(statistics.ordered))
        return(statistics.ordered(x, ...))

    if (is.function(atable_options("statistics.ordered")))
        return(atable_options("statistics.ordered")(x, ...))


    return(statistics(factor(x, ordered = FALSE), ...))
}

#' @export
#' @describeIn statistics Returns the \code{\link[base]{length}} of \code{x}. For class \code{'count_me'} see \code{\link{add_observation_column}}.

statistics.count_me <- function(x, ...) {

    statistics_out <- list(unused_name = length(x))
    names(statistics_out) <- atable_options("colname_for_observations")

    class(statistics_out) <- c("statistics_count_me", "list")
    return(statistics_out)
}
