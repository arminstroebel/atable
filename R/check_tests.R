#' Checks the output of functions two_sample_htest and multi_sample_htest
#'
#'
#' Checks the output of function \code{\link{two_sample_htest}} and \code{\link{multi_sample_htest}}.
#'
#'
#'
#' @param x Result of function \code{two_sample_htest} or \code{multi_sample_htest}.
#' @return \code{TRUE} if \code{x} has the following properties:
#' \code{x} is a named list with length > 0.
#' The names of the list must not have duplicates.
#' The names may contain NA. Else an error.
#'
#' Most hypothesis-test-functions in R like \code{\link[stats]{t.test}} or \code{\link[stats]{chisq.test}} return an object of class htest.
#' This object passes this checks.
#' Additional fields can be added to these objects and they will still pass this check.



check_tests <- function(x) {
    stopifnot(is.list(x), length(x) > 0)

    the_names <- names(x)
    stopifnot(!is.null(the_names), anyDuplicated(the_names) == 0)

    return(TRUE)
}
