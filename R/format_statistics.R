#' Format statistics
#'
#' The results of function \code{statistics} must be formated before printing. \code{format_statistics} does this.
#'
#' This function defines which statistics are printed in the final table and how they are formated.
#'
#' The format depends on the class \code{x}. See section methods.
#'
#' If you are not pleased with the current format you may alter these functions.
#' But you must keep the original output-format, see section Value.
#' Function \code{\link{check_format_statistics}} checks if the output of statistics is suitable for further processing.

#' @param x An object.
#' @param ... Passed from and to other methods.
#' @param format_statistics.statistics_numeric Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::format_statistics.statistics_numeric}.
#'   The function must mimic \code{\link{format_statistics}}: arguments are \code{x} and the ellipsis ... .
#'   Result is a non-empty data.frame with 2 columns called \code{'tag'} and \code{'value'}.
#' @param format_statistics.statistics_factor Analog to argument format_statistics.statistics_numeric
#'
#' @return
#' A non-empty data.frame with 2 columns called \code{'tag'} and \code{'value'}.
#' Column \code{'tag'} has class factor and no duplicates.
#' Column \code{'value'} is a character.
#' See also function \code{\link{check_format_statistics}}.



#' @export
format_statistics <- function(x, ...) {
    UseMethod("format_statistics")
}

#' @export
#' @describeIn format_statistics Defines how to format class \code{statistics_numeric}.
#' Returns a data.frame with 2 rows.
#' Column \code{'tag'} contains \code{'Mean_SD'} and \code{'valid_missing'}.
#' Column \code{'value'} contains two values: first value is the rounded mean and standard deviation, pasted them together. The standard deviation is bracketed.
#' Second value is the number of non-missing and missing values pasted together. The number of missing values is bracketed.

format_statistics.statistics_numeric <- function(x, format_statistics.statistics_numeric = NULL,
    ...) {


    if (is.function(format_statistics.statistics_numeric))
        return(format_statistics.statistics_numeric(x, ...))

    if (is.function(atable_options("format_statistics.statistics_numeric")))
        return(atable_options("format_statistics.statistics_numeric")(x, ...))



    the_mean <- atable_options("format_numbers")(x$mean)
    the_sd <- atable_options("format_numbers")(x$sd)

    values <- c(Mean_SD = paste0(the_mean, " (", the_sd, ")"), valid_missing = paste0(atable_options("format_numbers")(x$length -
        x$missing), " (", atable_options("format_numbers")(x$missing), ")"))

    format_statistics_out <- data.frame(tag = factor(names(values), levels = names(values)),
        value = values, row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE,
        fix.empty.names = FALSE)


    format_statistics_out$tag <- plyr::mapvalues(format_statistics_out$tag, from = "Mean_SD",
        to = atable_options("labels_Mean_SD"))
    format_statistics_out$tag <- plyr::mapvalues(format_statistics_out$tag, from = "valid_missing",
        to = atable_options("labels_valid_missing"))


    return(format_statistics_out)
}

#' @export
#' @describeIn format_statistics Defines how to format class \code{statistics_factor}.
#' Returns a data.frame.
#' Column \code{'tag'} contains all names of \code{x}.
#' Column \code{'value'} contains the percentages and the total number of values in brackets.

format_statistics.statistics_factor <- function(x, format_statistics.statistics_factor = NULL,
    ...) {

    if (is.function(format_statistics.statistics_factor))
        return(format_statistics.statistics_factor(x, ...))

    if (is.function(atable_options("format_statistics.statistics_factor")))
        return(atable_options("format_statistics.statistics_factor")(x, ...))


    nn <- names(x)

    value <- unlist(x)
    total <- sum(value)


    percent <- 100 * value/total

    # I use sapply(format, ...) instead just format because: ?format says: enough
    # decimal places will be used so that the smallest (in magnitude) number has this
    # many significant digits.  If you have numbers of different magnitudes, the
    # greatest of them will have a lot of digits. sapply ensures that all numbers
    # have the same number of digits, irrespective of the other numbers.
    value <- paste0(atable_options("format_percent")(percent), "% (", atable_options("format_numbers")(value), ")")

    format_statistics_out <- data.frame(tag = factor(nn, levels = nn), value = value,
        row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    return(format_statistics_out)

}


#' @export
#' @describeIn format_statistics Defines how to format class \code{statistics_count_me}.
#' Returns a data.frame.
#' Column \code{'tag'} contains the empty character \code{''}.
#' The empty character is choosen because \code{colname_for_observations} already appears in the final table.
#' Column \code{'value'} contains the number of observations.
#' See also \code{'colname_for_observations'} in \code{\link{atable_options}}.
format_statistics.statistics_count_me <- function(x, ...) {
    return(data.frame(tag = factor(""), value = atable_options("format_numbers")(x[[atable_options("colname_for_observations")]]),
        stringsAsFactors = FALSE))
}

#' @export
#' @describeIn format_statistics Returns a data.frame.
#' Column \code{'tag'} contains all names of \code{x}.
#' Column \code{'value'} contains all elements of \code{x}, rounded by \code{\link[base]{format}}.
format_statistics.default <- function(x, ...) {
    nn <- names(x)
    value <- unlist(x)

    value <- atable_options("format_numbers")(value)

    format_statistics_out <- data.frame(tag = factor(nn, levels = nn), value = as.character(value),
        row.names = NULL, stringsAsFactors = FALSE)
    return(format_statistics_out)
}
