#' Checks if valid name
#'
#' Checks for valid names by \code{\link[base]{make.names}},
#' i.e. \code{x} is valid iff \code{make.names} does nothing with \code{x}.
#'
#'
#' @param x An object.
#' @return A logical with length 1. \code{TRUE} when \code{x} is a character with length > 0 without duplicates
#' and is valid. Else \code{FALSE} and a warning what's wrong.
#'
#' @examples
#' x <- c('asdf', NA,'.na', '<y', 'asdf', 'asdf.1')
#' is_syntactically_valid_name(x)
#' is_syntactically_valid_name(x[FALSE]) # FALSE because empty
#' is_syntactically_valid_name(NA) # FALSE because not character
#' is_syntactically_valid_name(as.character(NA)) # FALSE because NA
#' is_syntactically_valid_name('NA') # FALSE. make.names changes 'NA' to 'NA.'
#' is_syntactically_valid_name(letters) # TRUE

#' @export
is_syntactically_valid_name <- function(x) {
    if (!is.character(x)) {
        warning("Not a character")
        return(FALSE)
    }

    if (length(x) == 0) {
        warning("Must have length > 0")
        return(FALSE)
    }

    not_unique <- duplicated(x)
    if (any(not_unique)) {
        warning("Not unique: ", paste(x[not_unique], collapse = " "))
        return(FALSE)
    }


    b <- mapply(identical, x = x, y = make.names(x, unique = TRUE, allow_ = TRUE),
        USE.NAMES = FALSE)

    if (any(!b)) {
        warning("Not valid: ", paste(x[!b], collapse = " "))
        return(FALSE)
    }

    return(TRUE)
}
