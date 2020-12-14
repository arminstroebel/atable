#' A longitudinal version of atable
#'
#' This is a wrapper for atable(), calculating the same statistics, but with different format.
#'
#' The intention is to report longitudinal data, i.e. data measured on the same objects on multiple times points.
#'
#' This function allows only one target_col and only one split_col (the time point of the measurement).
#'
#' The longitudinal formatting is:
#'
#' The names of the target_col and split_col do not show up in the table. The names should thus be written in the caption of the table.
#'
#'
#' Numeric target_cols get one line in the table; the format of the statistics is: mean (sd), N, missing.
#'
#' Factor target_cols also get one line in the table, when it has only two levels and only the first level
#' is displayed in the table and the name of the variable is omitted. This is intended for item like "Sex at birth: Female/Male".
#' Knowing the percentage of Female is sufficient in this case (when NAs are not counted).
#' The name of the target_cols and its first level should be stated in the caption of the table, otherwise the table is uninformative.
#' The format of the statistics is: percent % (n/total).
#'
#' Factors with three or more levels get one line per level and the name of the variable is omitted.
#' The format of the statistics is: percent % (n). The total number of obsercations is shown in an extra row.
#'
#' Argument block must omitted, as there is only one target_col and nothing to block.
#'
#' See examples.
#'
#' @param x object passed to atable. Currently x must be a data.frame.
#' @param target_cols character. Exactly one of colnames(x).
#' @param split_cols character. Exactly one of colnames(x).
#' @param group_col character or NULL. If character then, one of colnames(x).
#'
#' @param format_factor a function that defines the format of factor variables.
#' Default is defined in \code{\link{atable_options}}. See \code{\link{check_format_statistics}} for the return-value of this function.
#' @param format_numeric a function that defines the format of numeric variables.
#' Analog to format_factor.
#' @param ... Passed to \code{\link{atable}}.
#'
#' @return data.frame
#'
#'@examples
#' # create data with a time-variable
#' x = atable::test_data
#' set.seed(42)
#' x = within(x, {time = sample(paste0("time_", 1:5), size=nrow(x), replace = TRUE)})
#' split_cols = "time"
#' group_col = "Group2"
#'
#' # table for a factor with two levels
#' atable_longitudinal(x,
#'   target_cols = "Split2",
#'   group_col = group_col,
#'   split_cols = split_cols,
#'   add_margins = TRUE)
#'
#'
#' # table for a factor with three levels
#' atable_longitudinal(x,
#'   target_cols = "Split1",
#'   group_col = group_col,
#'   split_cols = split_cols,
#'   add_margins = TRUE)
#'
#'
#' # table for a numeric variable
#' atable_longitudinal(x,
#'   target_cols = "Numeric",
#'   group_col = group_col,
#'   split_cols = split_cols,
#'   add_margins = TRUE)
#'
#' # To print the table in Word or with Latex, use
#' # e.g. \link[Hmisc]{latex} or \link[officer]{body_add_table}.
#' # No further modification of the table is needed.
#' # See \code{\link{atable_compact}} for examples.
#'
#'
#' @export
atable_longitudinal = function(x, ...)
{
  UseMethod("atable_longitudinal")
}


#' @export
#' @describeIn atable_longitudinal a longitudinal version of atable.
atable_longitudinal.data.frame = function(x,
                                          target_cols,
                                          split_cols,
                                          group_col = NULL,
                                          format_numeric = atable_options("format_statistics_longitudinal.statistics_numeric"),
                                          format_factor = atable_options("format_statistics_longitudinal.statistics_factor"),
                                          ...)
{


  stopifnot(length(target_cols) == 1,
            length(split_cols) == 1)

  tab = atable(x = x,
               target_cols = target_cols,
               group_col = group_col,
               split_cols = split_cols,
               blocks = NULL,
               indent = FALSE,
               format_statistics.statistics_numeric = format_numeric,
               format_statistics.statistics_factor = format_factor,
               ...)


  if(is.factor(x[[target_cols]]) &&  nlevels(x[[target_cols]])>=3)
  {
    # factor with more than three levels
    tab = tab[setdiff(colnames(tab), atable_options("colname_for_variable")) ]
    tab[[split_cols]] = as.character(tab[[split_cols]])
    tab[[split_cols]] = replace_consecutive(tab[[split_cols]], by = NA)

    # class for printing in console
    class(tab) <- c("atable", "data.frame")
    tab[is.na(tab)] <- ""

    rownames(tab) <- NULL

    return(tab)
  }



  # just one line per variable, as defined by the format_statistics_longitudinal-methods
  # remove columns variable___  and tag

  cols = setdiff(colnames(tab), c(atable_options("colname_for_variable"),"tag"))
  b = !tab[[atable_options("colname_for_variable")]] %in% atable_options("colname_for_observations")

  tab = tab[b, cols]



  # class for printing in console
  class(tab) <- c("atable", "data.frame")
  tab[is.na(tab)] <- ""

  rownames(tab) <- NULL
  return(tab)

}



