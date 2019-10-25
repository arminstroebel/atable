#' Checks the output of function statistics
#'
#'
#' Checks the output of function \code{\link{statistics}}.
#'
#'
#'
#' @param  x Result of function \code{statistics}.
#' @return \code{TRUE} if \code{x} has the following properties:
#' \code{x} is a named list with length > 0.
#' The names of the list must not have duplicates.
#' The names may contain NA. Else an error.



check_statistics <- function(x) {
    stopifnot(is.list(x), length(x) > 0)

    the_names <- names(x)
    stopifnot(!is.null(the_names), anyDuplicated(the_names) == 0)

    return(TRUE)
}
