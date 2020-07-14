#' Replaces consecutive elements
#'
#' If \code{x[i+1]=x[i]} then \code{x[i+1]} is replaced by \code{by} for \code{i=1,...length(x)-1}.
#'
#' The \code{=} is defined by function \code{\link[base]{identical}} by default.
#' This function can be changed by argument fun_for_identical
#'
#'
#' @param x A character or factor.
#' @param by A character with length 1.
#' @param fun_for_identical A function with two arguments called \code{x} and \code{y}.
#'
#' @return A character, same length as \code{x}, now with consecutives replaced by \code{by}.
#' If \code{length(x) < 2}, x is returned unchanged.
#'
#' @examples
#' x <- rep(c('a','b','c','d'), times=c(2,4,1,3))
#' x
#' \dontrun{replace_consecutive(x)}
#' # NA should not be identical. So change fun_for_identical
#' fun_for_identical <- function(x,y) !is.na(x) && !is.na(y) && identical(x,y)
#' x <- c(1,1,3,3,NA,NA, 4)
#' x
#' \dontrun{replace_consecutive(x, by="99")}
#' \dontrun{replace_consecutive(x, by="99", fun_for_identical = fun_for_identical)}
#'
replace_consecutive <- function(x, by = "", fun_for_identical = base::identical )
{
  if (length(x) < 2) {
    return(x)
  }

  # look for consecutives
  is_consecutive <- mapply(fun_for_identical, x = x[1:length(x) - 1], y = x[2:length(x)],
                           USE.NAMES = FALSE)
  is_consecutive <- c(FALSE, is_consecutive)  # first entry is never consecutive

  x[is_consecutive] <- by
  return(x)

}
