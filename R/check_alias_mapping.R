#' Checks the output of function create_alias_mapping
#'
#' Checks the output of function \code{\link{create_alias_mapping}}.
#'
#'@param  Alias_mapping Result of function \code{create_alias_mapping}.
#'@return  \code{TRUE} if \code{x} has the following properties:
#' \code{Alias_mapping} is a non-empty data.frame with character columns \code{'old'} and \code{'new'}, without NA and "".
#' Column \code{'new'} has no duplicates.
#' Else throws an error. Prints the duplicates of column \code{'new'}, if available.



check_alias_mapping = function(Alias_mapping)
{

  stopifnot(
    is.data.frame(Alias_mapping),
    nrow(Alias_mapping)>0,
    c("old","new") %in% colnames(Alias_mapping),
    is.character(Alias_mapping$new),
    is.character(Alias_mapping$old),
    !is.na(Alias_mapping$new),
    !is.na(Alias_mapping$old),
    nchar(Alias_mapping$new)>0,
    nchar(Alias_mapping$old)>0
  )

  # show duplicated aliases to the user:
  b = duplicated(Alias_mapping$new, fromLast = TRUE) | duplicated(Alias_mapping$new, fromLast = FALSE)


  if(any(b)){
    alias_clash = Alias_mapping[b, c("old", "new")]


    print("name clash in aliases:")
    print( alias_clash)


    stop("These variables have duplicated aliases: ", paste0(alias_clash$old, collapse = ", "),
         ". Common alias: ", paste0(unique(alias_clash$new), collapse = ", "),
         ". Please change aliases.")
  }

  return(TRUE)


}
