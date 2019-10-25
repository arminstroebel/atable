#' Replaces NA
#'
#' Replaces \code{NA} in characters, factors and data.frames.
#'
#' The atable package aims to create readable tables. For non-computer-affine
#' readers \code{NA} has no meaning. So \code{replace_NA} exists.
#'
#' Methods for character, factor, ordered, list and data.frame available.
#' Default method returns \code{x} unchanged.
#'
#' Gives a warning when \code{replacement} is already present in \code{x} and
#' does the replacement.
#'
#' Silently returns \code{x} unchanged when there are no \code{NA} in \code{x}.
#'
#' Silently returns \code{x} unchanged when replacement is not a character of
#' length 1 or when replacement is \code{NA}.
#'
#' @param x An object.
#' @param replacement A character of length 1. Default value is defined
#'   in \code{atable_options('replace_NA_by')}, see \code{\link{atable_options}}.
#' @param ... Passed to methods.
#'
#' @return Same class as \code{x}, now with \code{NA} replaced by \code{replacement}.
#'
#' @examples
#' Character <- c(NA,letters[1:3], NA)
#' Factor <- factor(Character)
#' Ordered <- ordered(Factor)
#' Numeric <- rep(1, length(Factor))
#' Factor_without_NA <- factor(letters[1:length(Factor)])
#'
#' DD <- data.frame(Character, Factor, Ordered,
#'                 Numeric, Factor_without_NA,
#'                 stringsAsFactors = FALSE)
#' \dontrun{
#' DD2 <- replace_NA(DD, replacement = 'no value')
#'
#' summary(DD)
#' summary(DD2) # now with 'no value' instead NA in column Character, Factor and Ordered
#'
#' atable_options(replace_NA_by = 'not measured') # use atable_options to set replacement
#' DD3 <- replace_NA(DD)
#' summary(DD3) # now with 'not measured' instead NA
#'
#' atable_options_reset() # set 'replace_NA_by' back to default
#' }




#' @export
replace_NA <- function(x, ...) {
    UseMethod("replace_NA")
}

#' @export
#' @describeIn replace_NA replaces \code{NA} with \code{replacement}.
replace_NA.character <- function(x, replacement = atable_options("replace_NA_by"),
    ...) {
    if (is.character(replacement) && length(replacement) == 1 && !is.na(replacement)) {
        if (replacement %in% x) {
            warning("replacement ", replacement, " already in x: ", unique(x), ". Replacing anyway")
        }

        plyr::mapvalues(x, from = NA, to = replacement, warn_missing = FALSE)
    } else {
        # silently do nothing.
        return(x)
    }
}

#' @export
#' @describeIn replace_NA applies \code{replace_NA} to the levels of the factor. A
#'   factor with length > 0 without levels will get the level \code{replacement}.
replace_NA.factor <- function(x, ...) {
    if (any(is.na(x))) {

        x <- addNA(x)
        levels(x) <- replace_NA(levels(x), ...)

        return(x)
    } else {
        # silently do nothing
        return(x)
    }
}

#' @export
#' @describeIn replace_NA as factor.
replace_NA.ordered <- function(x, ...) {
    ordered(replace_NA(factor(x, ordered = FALSE), ...))
}

#' @export
#' @describeIn replace_NA applies \code{replace_NA} to all columns.
replace_NA.data.frame <- function(x, ...) {
    x[] <- lapply(x, replace_NA, ...)
    return(x)
}

#' @export
#' @describeIn replace_NA applies \code{replace_NA} to all elements of the list.
replace_NA.list <- function(x, ...) {
    lapply(x, replace_NA, ...)
}

#' @export
#' @describeIn replace_NA return \code{x} unchanged.
replace_NA.default <- function(x, ...) {
    return(x)
}
