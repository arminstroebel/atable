#' Replaces consecutive elements
#'
#' If \code{x[i+1]=x[i]} then \code{x[i+1]} is replaced by \code{by} for \code{i=1,...length(x)-1}.
#'
#' The \code{=} is defined by function \code{\link[base]{identical}}.
#'
#'
#' @param x A character or factor.
#' @param by A character with length 1.
#'
#' @return A character, same length as \code{x}, now with consecutives replaced by \code{by}.
#' If \code{length(x) < 2}, x is returned unchanged.
#'
#' @examples
#' x <- rep(c('a','b','c','d'), times=c(2,4,1,3))
#' x
#' \dontrun{replace_consecutive(x)}
#'
replace_consecutive <- function(x, by = "") {
    if (length(x) < 2) {
        return(x)
    }

    # look for consecutives
    is_consecutive <- mapply(identical, x = x[1:length(x) - 1], y = x[2:length(x)],
        USE.NAMES = FALSE)
    is_consecutive <- c(FALSE, is_consecutive)  # first entry is never consecutive

    x[is_consecutive] <- by
    return(x)

}
