#' Get Aliases of column names
#'
#' Column names of data.frame in atable must have syntactically valid colnames,
#' see \code{\link[atable]{is_syntactically_valid_name}}.
#' So no blanks or special characters allowed.
#' But Reporting in human readable language needs special characters.
#' These functions here allow atable to handle arbitrary character for pretty printing.
#'
#'
#' We use \code{\link[base]{attributes}} here, to assign alternative names to columns.
#' Also class \code{labelled} created by Hmisc's \code{\link[Hmisc]{label}} is supported.
#'
#' See \code{create_alias_mapping} for the function that does the actual work.
#'
#' If no aliases are found, then underscores in the column names of \code{DD} will be replaced by blanks.
#' See Examples in \code{?atable}.
#'
#' @param DD A data.frame
#' @param ... Passed from and to other methods.
#'
#' @return
#' \code{create_alias_mapping} returns a data.frame with two columns \code{old} and \code{new} and
#' as many rows as \code{DD} has columns. Column \code{old} contains the original column names of
#' \code{DD} and column \code{new} their aliases.
#'
#' @export
create_alias_mapping <- function(DD, ...) {
    stopifnot(is.data.frame(DD))
    
    alias_list <- get_alias(DD, ...)
    
    
    check_alias <- function(x) {
        if (!(is.character(x) | is.null(x))) {
            stop("Function get_alias must return a character or NULL, but returned ", 
                x)
        }
    }
    
    unused <- lapply(alias_list, check_alias)
    
    
    alias_list <- sapply(alias_list, function(x) {
        if (is.null(x)) {
            NA
        } else {
            x
        }
    })
    # get_alias may return NULL, when NULL is coerced to character this will change
    # the length of the list. Thus replace NULL by NA.
    
    
    colnames_without_underscore <- sapply(colnames(DD), atable_options("modifiy_colnames_without_alias"), 
        ..., USE.NAMES = FALSE)
    
    Alias_mapping <- data.frame(old = colnames(DD), alias_from_attr = as.character(alias_list), 
        colnames_without_underscore = colnames_without_underscore, stringsAsFactors = FALSE)
    
    
    
    Alias_mapping$new <- ifelse(is.na(Alias_mapping$alias_from_attr), Alias_mapping$colnames_without_underscore, 
        Alias_mapping$alias_from_attr)
    # I could use within{} here, but then devtools::check gives a note about 'no
    # binding of alias_from_attr'
    
    return(Alias_mapping)
    
}
