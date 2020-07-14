#' Get Aliases of column names
#'
#' Retrieves attributes \code{label} and \code{units} of class labelled and attribute \code{alias} otherwise.
#'
#' We use \code{\link[base]{attributes}} here, to assign alternative names to columns.
#' Also class \code{labelled} created by Hmisc's \code{\link[Hmisc]{label}} is supported.
#'
#' This is a workhorse function, see \code{create_alias_mapping} for the high level function
#'
#' @param x An object. Aliases will be retrieved of \code{x}.
#' @param ... Passed from and to other methods.
#'
#' @return
#' For atomic vectors a character of \code{NULL}; for non-atomic vectors the results of \code{get_alias} applied to its elements.

#' @export
get_alias <- function(x, ...) {
    UseMethod("get_alias")
}


#' @export
#' @describeIn get_alias Retrieve attributes \code{label} and \code{units}, if available.
#' Units are bracketed by '[]'. See also \code{\link[Hmisc]{label}} and \code{\link[Hmisc]{units}}.
#' The user may alter this method via \code{\link{atable_options}}, see help there.
get_alias.labelled <- function(x, ...) {
    atable_options("get_alias.labelled")(x, ...)
}



#' @export
#' @describeIn get_alias Retrieve attribute \code{alias} via \code{\link[base]{attr}}.
#' This attribute may be an arbitrary character.
#' If there is no attribute \code{alias}, then \code{get_alias.default} returns \code{NULL}.
get_alias.default <- function(x, ...) {
    atable_options("get_alias.default")(x, ...)
}

#' @export
#' @describeIn get_alias Calls \code{get_alias} on every column.
get_alias.data.frame <- function(x, ...) {
    lapply(x, get_alias, ...)
}

#' @export
#' @describeIn get_alias Calls \code{get_alias} on every element of the list.
get_alias.list <- function(x, ...) {
    lapply(x, get_alias, ...)
}


