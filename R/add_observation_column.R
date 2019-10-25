#' Adds a column to a data.frame
#'
#' The new column has name \code{atable_options('colname_for_observations')} and class \code{'count_me'}.
#'
#' Throws an error if a column of that name is already present in \code{DD}.
#'
#' @param  DD A data.frame.
#' @return As DD now with one more column.
#'

add_observation_column <- function(DD) {
    stopifnot(is.data.frame(DD))
    if (atable_options("colname_for_observations") %in% colnames(DD)) {
        stop("Name clash. ", atable_options("colname_for_observations"), " is already in colnames(DD). Please change
             atable_options('colname_for_observations') ")
    }

    DD[[atable_options("colname_for_observations")]] <- integer(nrow(DD))

    class(DD[[atable_options("colname_for_observations")]]) <- c("count_me", "integer")

    return(DD)
}
