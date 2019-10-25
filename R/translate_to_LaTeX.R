#' A wrapper for latexTranslate
#'
#' Translate_to_LaTeX calls \code{\link[Hmisc:latex]{latexTranslate}}.
#'
#' Result is suitable for print with \code{\link[Hmisc]{latex}}.
#'
#' Translate_to_LaTeX uses S3 object system. See seection methods.
#'
#' @param x An object.
#' @param inn,out,pb,greek,na,... As in \code{\link[Hmisc]{latex}}.
#'
#' @return Same length as \code{x}, now translated to latex.
#'
#'
#' @export
translate_to_LaTeX <- function(x, ...) {
    UseMethod("translate_to_LaTeX")
}

#' @export
#' @describeIn translate_to_LaTeX Applies \code{\link[Hmisc:latex]{latexTranslate}} to
#' \code{rownames(x)}, \code{colnames(x)} and all columns of \code{x}.
translate_to_LaTeX.data.frame <- function(x, ...) {
    DD <- x

    DD <- if (nrow(DD) == 0) {
        colnames(DD) <- translate_to_LaTeX(colnames(DD))
        return(DD)
    } else {

        new_rownames <- translate_to_LaTeX(rownames(DD))

        DD[] <- lapply(DD, translate_to_LaTeX, ...)


        colnames(DD) <- translate_to_LaTeX(colnames(DD), ...)
        rownames(DD) <- new_rownames

        return(DD)

    }
}

#' @export
#' @describeIn translate_to_LaTeX Translates all elements of \code{x}.
translate_to_LaTeX.list <- function(x, ...) {
    lapply(x, translate_to_LaTeX, ...)
}

#' @export
#' @describeIn translate_to_LaTeX As \code{\link[Hmisc:latex]{latexTranslate}}.
translate_to_LaTeX.character <- function(x, inn = NULL, out = NULL, pb = FALSE, greek = FALSE,
    na = "", ...) {
    Hmisc::latexTranslate(x, inn = inn, out = out, pb = pb, greek = greek, na = na,
        ...)
}

#' @export
#' @describeIn translate_to_LaTeX Casts to character and then translates.
translate_to_LaTeX.numeric <- function(x, ...) {
    translate_to_LaTeX(as.character(x), ...)
}

#' @export
#' @describeIn translate_to_LaTeX Translates the levels of the factor.
translate_to_LaTeX.factor <- function(x, ...) {
    new_levels <- translate_to_LaTeX(levels(x), ...)

    return(plyr::mapvalues(x, from = levels(x), new_levels))
}

#' @export
#' @describeIn translate_to_LaTeX Casts to character and then translates.
translate_to_LaTeX.logical <- function(x, ...) {
    translate_to_LaTeX(as.character(x), ...)
}
