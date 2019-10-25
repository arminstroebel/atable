#' Checks the output of function format_statistics
#'
#' Checks the output of function \code{\link{format_statistics}}.
#'
#'@param  x Result of function \code{format_statistics}.
#'@return  \code{TRUE} if \code{x} has the following properties:
#' \code{x} is a non-empty data.frame with 2 columns called \code{'tag'} and \code{'value'}.
#' Column \code{'tag'} has class factor and no duplicates.
#' Column \code{'value'} is a character.
#' Else throws an error.
#'
check_format_statistics <- function(x) {
    stopifnot(is.data.frame(x), nrow(x) > 0, colnames(x) == c("tag", "value"), is.factor(x$tag),
        anyDuplicated(x$tag) == 0, is.character(x$value))

    return(TRUE)
}
