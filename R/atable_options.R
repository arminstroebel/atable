# This file is based on https://cran.r-project.org/web/packages/settings/vignettes/settings.html MYPKGOPTIONS Variable, global to
# package's namespace.  This function is not exported to user space and does not need to be documented.
MYPKGOPTIONS <- settings::options_manager(
  add_margins = FALSE,
  colname_for_total = "Total",
  replace_NA_by = "missing",
  colname_for_variable = "variable___",
  colname_for_observations = "Observations",
  colname_for_group = "Group",
  colname_for_value = "value",
  colname_for_blocks = "block_name___",
  colname_for_order = "order___",
  labels_TRUE_FALSE = c("yes", "no"),
  labels_Mean_SD = "Mean (SD)",
  labels_valid_missing = "valid (missing)",
  format_to = "Latex",
  statistics.numeric = NULL,
  statistics.factor = NULL,
  statistics.ordered = NULL,
  two_sample_htest.factor = NULL,
  two_sample_htest.numeric = NULL,
  two_sample_htest.ordered = NULL,
  multi_sample_htest.factor = NULL,
  multi_sample_htest.numeric = NULL,
  multi_sample_htest.ordered = NULL,
  format_statistics.statistics_factor = NULL,
  format_statistics.statistics_numeric = NULL,
  format_tests.htest = NULL,
  format_tests.htest_with_effect_size = NULL,
  format_p_values = function(x){
    if (!is.nan(x) & x < 0.001){
      return("<0.001")
    }else{
      return(sapply(x, format, scientific = FALSE, digits = 2, trim = TRUE, nsmall = 0))
      }
    },
  format_numbers = function(x){

    # I want scientific notation when x has more than 3 digits to base 10
      scientific <- sapply(x, function(x)is.numeric(x) && is.finite(x) && x != 0 && abs(log10(abs(x))) > 3)

      return(mapply(format, x = x, scientific = scientific,
             MoreArgs = list(digits = atable_options("digits"), trim = TRUE, nsmall = 0)))
    },
  format_percent = function(x){
    return(sapply(x, format, scientific = FALSE, digits = atable_options("digits"), trim = TRUE, nsmall = 0))
    },
  digits = 2,
  get_alias.default = function(x, ...){
    attr(x, "alias", exact = TRUE)
  },
  modifiy_colnames_without_alias = function(x, ...){
    x = gsub(x, pattern = "_", replacement = " ")
    x = trimws(x, which ="both")
    return(x)},
  get_alias.labelled = function(x, ...){

    out <- attr(x, "label", exact = TRUE)

    Units <- attr(x, "units", exact = TRUE)

    out = if(!is.null(Units)){
      paste0(out, " [", Units, "]")}else{out}

    return(out)

  }


)
# User function that gets exported:
#' Set or get options
#'
#' Set or get options for the atable-package via the \code{\link[settings]{settings}} package.
#'
#' These options control some aspects of the atable package.
#'
#' For restoring the default values see \code{\link{atable_options_reset}}.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{add_margins}}{: A logical with length 1, TRUE of FALSE. This is the default-value of atable's
#'  argument \code{add_margins}. See the help there.}
#'
#'  \item{\code{colname_for_total}}{: A character with length 1. Default is \code{'Total'}. This character will show up
#'  in the results of \code{\link{atable}} when \code{add_margins} is \code{TRUE} and \code{group_col} is not \code{NULL}.}
#'
#'  \item{\code{replace_NA_by}}{: A character with length 1, or \code{NULL}. Default is \code{'missing'}.
#'  Used in function \code{\link{replace_NA}}. This character will show up in the results of \code{\link{atable}},
#'  so it can be modified. }
#'
#'  \item{\code{colname_for_variable}}{: A character with length 1. Default is \code{'variable___'}.
#'  Used in function \code{add_name_to_tests} and \code{add_name_to_statistics}.
#'  This character will not show up in the results and is only used internally for intermediate data.frames.
#'  There may be name clashes with user-supplied data.frames; so modification may be necessary.}
#'
#'  \item{\code{colname_for_observations}}{: A character with length 1. Default is \code{'Observations'}.
#'  Used in function \code{add_observation_column}.
#'   This character will show up in the results of \code{\link{atable}}, so it can be modified.
#'   There may be name clashes with user-supplied data.frames; so modification may be necessary.}
#'
#'   \item{\code{colname_for_blocks}}{: A character with length 1. Default is \code{'block_name___'}.
#'  Used in function \code{indent_data_frame_with_blocks}.
#'  This character will not show up in the results and is only used internally for intermediate data.frames.
#'  There may be name clashes with user-supplied data.frames; so modification may be necessary.}
#'
#'   \item{\code{labels_TRUE_FALSE}}{: A character of length 2. Default is \code{c('yes', 'no')}.
#'   Currently used in function \code{statistics.logical} (see \code{\link{statistics}}) to cast logical to factor.
#'   \code{TRUE} is mapped to \code{labels_TRUE_FALSE[1]} and \code{FALSE} to \code{labels_TRUE_FALSE[2]}.
#'   This characters may show up in the results of \code{\link{atable}}, so it can be modified.}
#'
#'   \item{\code{labels_Mean_SD}}{: A character length 1. Default is \code{'Mean (SD)'}.
#'   Currently used in function \code{\link{format_statistics}} as a name for the mean and standard deviation of
#'   numeric variables. This character may show up in the results of \code{\link{atable}}, so it can be modified.}
#'
#'   \item{\code{labels_valid_missing}}{: A character length 1. Default is \code{'valid (missing)'}.
#'   Currently used in function \code{\link{format_statistics}} as a name for the number of valid and missing values
#'   of numeric variables. This character may show up in the results of \code{\link{atable}}, so it can be modified.}
#'
#'   \item{\code{format_to}}{: A character length 1. Default is \code{'Latex'}.
#'   Currently used in function \code{\link{atable}}.}
#'
#'   \item{\code{colname_for_group}}{: A character of length 1. Default is \code{'Group'}.
#'   This character will show up in the results of \code{\link{atable}}.
#'   This column will contain all values of \code{DD[split_cols]} and \code{DD[target_cols]}.}
#'
#'   \item{\code{colname_for_value}}{: A character of length 1. Default is \code{'value'}.
#'   This character shows up in the results of \code{\link{atable}} when \code{group_col} is \code{NULL}.
#'   The column will contain the results of the \code{\link{statistics}}.}
#'
#'   \item{\code{statistics.numeric}}{: Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::statistics.numeric} when atable is called.
#'   The function must mimic \code{\link{statistics}}: see the help there.}
#'
#'   \item{\code{statistics.factor}}{: Analog to argument statistics.numeric.}
#'
#'   \item{\code{statistics.ordered}}{: Analog to argument statistics.numeric.}
#'
#'   \item{\code{two_sample_htest.numeric}}{: Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::two_sample_htest.numeric} when atable is called.
#'   The function must mimic \code{\link{two_sample_htest}}: see the help there.}
#'
#'   \item{\code{two_sample_htest.factor}}{: Analog to argument two_sample_htest.numeric}
#'
#'   \item{\code{two_sample_htest.ordered}}{: Analog to argument two_sample_htest.numeric}
#'
#'
#'   \item{\code{multi_sample_htest.numeric}}{: Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::multi_sample_htest.numeric} when atable is called.
#'   The function must mimic \code{\link{multi_sample_htest}}: see the help there.}
#'
#'   \item{\code{multi_sample_htest.factor}}{: Analog to argument multi_sample_htest.numeric}
#'
#'   \item{\code{multi_sample_htest.ordered}}{: Analog to argument multi_sample_htest.numeric}
#'
#'   \item{\code{format_statistics.statistics_numeric}}{: Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{atable:::format_statistics.statistics_numeric}.
#'   The function must mimic \code{\link{format_statistics}}: see the help there.}
#'
#'   \item{\code{format_statistics.statistics_factor}}{: Analog to argument format_statistics.statistics_numeric}
#'
#'   \item{\code{format_tests.htest}}{: Either \code{NULL} or a function. Default is \code{NULL}.
#'   If a function, then it will replace \code{format_tests.htest}.
#'   The function must mimic \code{\link{format_tests}}: arguments are \code{x} and the ellipsis ... .
#'   Result is a data.frame with 1 rows and unique colnames.}
#'
#'   \item{\code{format_tests.htest_with_effect_size}}{: Analog to argument format_tests.htest}
#'
#'
#'
#'   \item{\code{format_p_values}}{: A function with one argument returning a character with same length as the argument.
#'    This functions is called by \code{\link{format_tests}} to produce printable p-values.}
#'
#'   \item{\code{format_percent}}{: A function with one argument returning a character with same length as the argument.
#'    This functions is called by \code{\link{format_statistics}} for factors to produce printable percentages.}
#'
#'   \item{\code{format_numbers}}{: A function with one argument returning a character with same length as the argument.
#'    This functions is called by \code{\link{format_statistics}} and \code{\link{format_tests}} for number,
#'    that are not p-values or percentages.}
#'
#'   \item{\code{digits}}{: 2. How many digits a number should have in the table.
#'   Used by \code{format_percent} and \code{format_percent} and passed to \code{\link[base]{format}}. }
#'
#'    \item{\code{get_alias.default}}{: A function with one argument \code{x} and \code{...} returning a character or \code{NULL}.
#'    This functions is called by \code{get_alias} and \code{create_alias_mapping} to retrieve alternative Variable names to print
#'    in the table.}
#'
#'    \item{\code{get_alias.labelled}}{: A function with one argument \code{x} and \code{...}, that must return a character.
#'    This functions is called by \code{get_alias} on the columns that have class labelled.}
#'
#'    \item{\code{modifiy_colnames_without_alias}}{: A function with one argument \code{x} and \code{...} returning a character.
#'    This functions is called by \code{create_alias_mapping} on the columns that have \code{is.NULL(get_alias(x))}.
#'    Replaces underscores by blanks and then calls \code{\link[base]{trimws}}. }
#' }
#'
#' @examples

#' atable_options() # show all options
#' atable_options('replace_NA_by' = 'no value') # set a new value
#' atable_options('replace_NA_by') # return the new value
#'
#'
#' @export
atable_options <- function(...) {
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  MYPKGOPTIONS(...)
}
