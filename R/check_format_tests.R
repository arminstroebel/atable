#' Checks the output of functions format_test
#'
#' Checks the output of function \code{\link{format_tests}}.
#'
#' @param  x Result of function \code{format_tests}.
#' @return \code{TRUE} if \code{x} has the following properties:
#' \code{x} is a data.frame with exactly one row and with unique colnames. Else throws an error.
#'
check_format_tests <- function(x) {
    stopifnot(is.data.frame(x), nrow(x) == 1, ncol(x) > 0, anyDuplicated(colnames(x)) ==
        0, sapply(x, is.atomic))

    return(TRUE)
}
