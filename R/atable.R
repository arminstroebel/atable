#' Create Tables for Reporting of Clinical Trials
#'
#' Applies descriptive statistics and hypothesis tests to data, and arranges the results for printing.
#'
#' @param x An object. If \code{x} is a data.frame, it must have unique and syntactically valid colnames,
#' see \code{\link[atable]{is_syntactically_valid_name}}. If \code{x} is a formula, then its format must
#' be \code{target_cols ~ group_col | split_cols}. See other arguments for more details.
#'
#' @param data Passed to \code{atable(x = data, ...)}.
#'
#' @param target_cols A character vector containing some column names of \code{x}.
#'
#' Descriptive statistics and hypothesis test are applied to these columns depending on their class.
#' The descriptive statistics are defined by \code{\link{statistics}};
#' their representation and format by \code{\link{format_statistics}}.
#'
#' Hypothesis test are defined by \code{\link{two_sample_htest}} or \code{\link{multi_sample_htest}}
#' (depending on the number of levels of \code{group_col});
#' their representation and format by \code{\link{format_tests}}.
#' Note that atable always adds one name to \code{target_cols} to count the number of obsservations.
#' This name is stored in \code{atable_options('colname_for_observations')}.
#'
#' @param group_col A character of length 1 containing a column of \code{x} or \code{NULL}.
#' This column defines the groups that are compared by the hypothesis tests.
#' \code{\link[base:factor]{as.factor}} is applied to this column before further processing.
#' Default is \code{NULL}, meaning that no hypothesis tests are applied.
#'
#' @param split_cols A character vector containing some of \code{colnames(x)} or \code{NULL}.
#' \code{x} is splitted by these columns before descriptive statistics and hypothesis test are applied.
#' \code{\link[base:factor]{as.factor}} is applied to this column before further processing.
#' Default is \code{NULL}, meaning that no splitting is done.
#'
#' @param format_to A character vector of length 1. Specifies the format of the output of \code{atable}.
#'  Possible values are \code{'Latex'}, \code{'Word'}, \code{'Raw'}, \code{'HTML'}, \code{'Console'},
#'  \code{'markdown'}, \code{'md'}.
#'  Default is defined in \code{\link{atable_options}}.
#'
#' @param drop_levels A logical. If \code{TRUE} then \code{\link[base]{droplevels}} is called on \code{group_col}
#'  and \code{split_cols} before further processing. Default is \code{TRUE}.
#'
#' @param add_levels_for_NA If \code{TRUE} then \code{\link[base:factor]{addNA}} is called on \code{group_col} and
#' \code{split_cols} before further processing. Default is \code{FALSE}.
#'
#' @param formula A formula of the form \code{target_cols ~ group_col | split_cols}.
#' The \code{|} separates the \code{group_col} from the \code{split_cols}.
#' Read the \code{|} as 'given' as in a conditional probability \code{P(target_cols | split_cols)}.
#' \code{target_cols} and \code{split_cols} may contain multiple names separated by \code{+}.
#' \code{group_col} must be a single name if given.
#' \code{group_col} and \code{split_cols} may be omitted and can be replaced by \code{1} in this case.
#' The \code{|} may also be omitted if no \code{split_cols} are given.
#' @param blocks \code{NULL} or a list. If blocks is a list, then the names of the list must be non-NA characters.
#' The elements of the list must be some of \code{target_cols}, retaining the order of \code{target_cols}.
#' Also in this case \code{split_cols} must be \code{NULL} as simultaneous blocking and splitting is not supported.
#' Default is \code{NULL}, meaning that no blocking is done. Variables of a block are additionally indented.
#' Blocking has no effect on the statistics, it only affects the indentation of the resulting table. See Examples.
#'
#' @param add_margins A logical with length one, \code{TRUE} or \code{FALSE}. Default is defined
#' in \code{\link{atable_options}} as \code{FALSE}. When \code{add_margins} is \code{TRUE} and
#' \code{group_col} is not \code{NULL}, a column containing the results of an ungrouped \code{atable}-call is added to
#'  the results. See Examples.
#'
#' @param indent_character A character with length 1 or \code{NULL} (default). This character is used for indentation in the resulting
#'   table. If \code{NULL}, then the value stored in \code{\link{atable_options}} is taken instead, depending on \code{format_to}.
#'  \code{\link{indent_data_frame}} does the indentation. See help there.
#'
#' @param indent A logical with length one, \code{TRUE} or \code{FALSE}. Default is defined
#' in \code{\link{atable_options}}. Decides if indentation is done or not. The resulting table will have a different layout.
#' If FALSE, then \code{blocks} is ignored.
#'
#' @param ... Passed from and to other methods. You can use the ellipsis ... to modify atable:
#' For example the default-statistics for numeric variables are mean and sd. To change these statistics pass
#' a function to argument \code{statistics.numeric}, that calculates the statistics you prefer for your data.
#'
#' See examples below how to modify atable by ... .
#'
#' Actually \code{statistics.numeric} is passed to \code{\link{statistics}} and thus documented there,
#' but for convenience it also documented here.
#'
#' Here is a list of the statistics and hypothesis tests that can be modified by \code{...} :
#' \itemize{
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
#'   The function must mimic \code{\link{format_tests}}: see the help there.}
#'
#'   \item{\code{format_tests.htest_with_effect_size}}{: Analog to argument format_tests.htest}
#'
#'
#'
#'  }
#'
#'
#' @return
#' Results depend on \code{format_to}:
#' \itemize{
#' \item{\code{'Raw'}: }{A list with two elemtents called \code{'statistics_result'} and \code{'tests_result'}, that
#' contain all results of the descriptve statistics and the hypothesis tests.
#' This format useful, when extracting a specific result unformated
#'  (when \code{format_to} is not \code{'Raw'} all numbers are also returned, but as rounded
#'  characters for printing and squeezed into a data.frame).
#' \itemize{
#'  \item{\code{'statistics_result'}: } { contains a data.frame with colnames \code{c(split_cols, group_col, target_cols}.
#'  \code{split_cols} and \code{group_col} retain their original values (now as factor).
#'  \code{target_cols} contain lists with the results of function \code{\link{statistics}}.
#'  As the result of function \code{statistics} is also a list, \code{target_cols} contain lists of lists.}
#'
#'    \item {\code{'tests_result'}: } {has the same structure as \code{'statistics_result'}, but contains the results
#'  of \code{\link{two_sample_htest}} and \code{\link{multi_sample_htest}}.
#'  Note that \code{tests_result} only exists if \code{split_cols} is not \code{NULL}.}}
#' }
#' \item{\code{'Word'}: }{A data.frame.
#' Column \code{atable_options('colname_for_group')} contains
#' all combinations of the levels of \code{split_cols} and
#' the names of the results of function \code{\link{format_statistics}}.
#'
#' Further columns are the levels of \code{group_col} the names of the results of \code{format_tests}.
#'
#' The levels of \code{split_cols} and the statistics are arranged vertically.
#' The hypothesis test are arranged horizontally.
#'
#' }
#' \item{\code{'HTML'}: }{Same as for \code{format_to = 'Word'} but a different character indents
#' the first column.}
#' #' \item{\code{'Console'}: }{Meant for printing in the R console for interactive analysis.
#' Same as for \code{format_to = 'Word'} but a different character indents the first column.}
#' \item{\code{'Latex'}: }{Same as for \code{format_to = 'Word'} but a different character indents
#' the first column and with \code{\link{translate_to_LaTeX}} applied afterwards. }
#' }
#' @examples
#' # See vignette for more examples:
#' # utils::vignette('atable_usage', package = 'atable')
#'
#' # Analyse datasets::ToothGrowth:
#' # Length of tooth for each dose level and delivery method:
#' atable::atable(datasets::ToothGrowth,
#'   target_cols = 'len',
#'   group_col = 'supp',
#'   split_cols = 'dose',
#'   format_to = 'Word')
#' # Print in .docx with e.g. flextable::regulartable and officer::body_add_table
#'
#' # Analyse datasets::ChickWeight:
#' # Weight of chickens for each time point and diet:
#' atable(weight ~ Diet | Time, datasets::ChickWeight, format_to = 'Latex')
#' # Print as .pdf with e.g. Hmisc::latex
#'
#' # Analyse atable::test_data:
#' atable(Numeric + Logical + Factor + Ordered ~ Group | Split1 + Split2,
#'   atable::test_data, format_to = 'HTML')
#'# Print as .html with e.g. knitr::kable and options(knitr.kable.NA = '')
#'
#' # Modify atable: calculate median and MAD for numeric variables
#' new_stats  <- function(x, ...){list(Median = median(x, na.rm = TRUE),
#'                                     MAD = mad(x, na.rm = TRUE))}
#' atable(atable::test_data,
#'        target_cols = c('Numeric', 'Numeric2'),
#'        statistics.numeric = new_stats,
#'        format_to = 'Console')
#' # Print in Console with format_to = 'Console'.
#'
#' # Analyse mtcars and add labels and units of via package Hmisc
#' mtcars <- within(datasets::mtcars, {gear <- factor(gear)})
#' # Add labels and units.
#' attr(mtcars$mpg, 'alias') = 'Consumption [Miles (US)/ gallon]'
#' Hmisc::label(mtcars$qsec) = 'Quarter Mile Time'
#' units(mtcars$qsec) = 's'
#'
#' # apply atable
#' atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
#'                mtcars,
#'                format_to = 'Console')
#'
#' # Blocks
#' # In datasets::mtcars the variables cyl, disp and mpg are related to the engine and am and gear are
#' # related to the gearbox. So grouping them together is desireable.
#' atable::atable(datasets::mtcars,
#'                target_cols = c("cyl", "disp", "hp", "am", "gear", "qsec") ,
#'                blocks = list("Engine" = c("cyl", "disp", "hp"),
#'                              "Gearbox" = c("am", "gear")),
#'                format_to = "Console")
#' # Note that Variable qsec is not blocked and thus not indented.
#'
#'
#'
#' # add_margins
#' atable::atable(atable::test_data,
#'                target_cols = "Numeric",
#'                group_col = "Group",
#'                split_cols = "Split1",
#'                add_margins = TRUE,
#'                format_to = "Console")
#' # The column 'Total' contains the results of the ungrouped atable-call:
#' # The number of observations is the sum of observations of the groups.
#' # The default of add_margins can be changed via atable_options.
#'





#' @export
atable <- function(x, ...) {
    UseMethod("atable")
}


#' @export
#' @describeIn atable applies descriptive statistics and hypothesis tests, arranges the results for printing.
atable.data.frame <- function(x, target_cols, group_col = NULL, split_cols = NULL,
    format_to = atable_options("format_to"), drop_levels = TRUE, add_levels_for_NA = FALSE, blocks = NULL,
    add_margins = atable_options("add_margins"), indent_character = NULL, indent = atable_options("indent"), ...) {

    format_to <- switch(format_to, Latex = "Latex", latex = "Latex", Word = "Word",
        word = "Word", HTML = "HTML", html = "HTML", Console = "Console", console = "Console",
        Raw = "Raw", raw = "Raw", md = "markdown", markdown = "markdown", "Console")


    DD <- x
    stopifnot(is_syntactically_valid_name(colnames(DD)), is.character(target_cols),
        is.character(format_to), length(format_to) == 1, length(target_cols) > 0,
        all(target_cols %in% colnames(DD)), is.null(group_col) || (is.character(group_col) &&
            length(group_col) == 1 && (group_col %in% colnames(DD))), is.null(split_cols) ||
            (is.character(split_cols) && all(split_cols %in% colnames(DD))), anyDuplicated(c(target_cols,
            group_col, split_cols)) == 0)



    DD <- DD[c(target_cols, split_cols, group_col)]  # only these columns are relevant

    # check blocks
    b <- check_blocks(blocks, target_cols)

    Alias_mapping <- create_alias_mapping(DD[c(target_cols, split_cols)])
    # I create Alias_mapping once here and pass it down to other functions, because
    # subsetting rows in DD removes attributes.  See Hmisc getAnywhere('[.labelled')
    # for a workaround. But I do not know all classes and hence cannot write [
    # methods for them.

    b = check_alias_mapping(Alias_mapping)

    # I cast group_col and split_cols to factor that allows more flexibility for the
    # input than only factor: grouping and splitting by character, numeric and
    # logical is possible
    DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], as.factor)

    DD[c(group_col, split_cols)] <- if (isTRUE(drop_levels)) {
        droplevels(DD[c(group_col, split_cols)])
    } else {
        DD[c(group_col, split_cols)]
    }

    # add one level to factors without a level factors without a level can happen if
    # a factor contains only NA and droplevels is applied
    DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], function(x) if (nlevels(x) ==
        0) {
        addNA(x)
    } else {
        x
    })



    # Explicitly include missing values of DD[c(group_col, split_cols)] in the
    # analysis (if any NA in DD[c(group_col, split_cols)]).
    DD <- if (isTRUE(add_levels_for_NA)) {
        DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], function(x) if (any(is.na(x))) {
            addNA(x)
        } else {
            x
        })
        DD
    } else {
        DD[stats::complete.cases(DD[c(group_col, split_cols)]), , drop = FALSE]
    }



    # I want to calculate the number of observations in every group.  I do this by
    # adding a column called atable_options('colname_for_observations') of class
    # 'count_me' to DD.  The package atable defines the function
    # 'statistics.count_me'. It just returns the length of the vector.
    DD <- add_observation_column(DD)


    # check blocks and target_cols
    if(!is.null(blocks)) {
      if( !is.null(split_cols) ){
        stop("When blocks is not NULL, then split_cols must be NULL. Blocking with split_cols is not supported.")}
      }

    # Check if group_col or split_cols are NULL and call the appropriate
    # atable-functions
    result <- if (is.null(group_col)) {
        if (is.null(split_cols)) {
            atable_unsplitted_ungrouped(DD = DD, target_cols = target_cols, format_to = format_to,
                Alias_mapping = Alias_mapping, blocks = blocks, indent_character = indent_character, indent = indent, ...)
        } else {
            atable_splitted_ungrouped(DD = DD, target_cols = target_cols, split_cols = split_cols,
                Alias_mapping = Alias_mapping, blocks = blocks, format_to = format_to, indent_character = indent_character, indent = indent, ...)
        }
    } else {
        if (is.null(split_cols)) {
            atable_unsplitted_grouped(DD = DD, target_cols = target_cols, group_col = group_col,
                Alias_mapping = Alias_mapping, split_cols = split_cols, format_to = format_to, blocks = blocks, indent_character = indent_character, indent = indent, ...)
        } else {
            atable_splitted_grouped(DD = DD, target_cols = target_cols, group_col = group_col,
                split_cols = split_cols, format_to = format_to, Alias_mapping = Alias_mapping, blocks = blocks, indent_character = indent_character, indent = indent, ...)
        }
    }

    # localization
    result <- if ("Group" %in% colnames(result)) {
        doBy::renameCol(result, "Group", atable_options("colname_for_group"))
    } else {
        result
    }
    result <- if ("value" %in% colnames(result)) {
        doBy::renameCol(result, "value", atable_options("colname_for_value"))
    } else {
        result
    }


    # add_margins
    result <- if(isTRUE(add_margins) & !is.null(group_col)){
      tab_no_group <- atable(x = x, target_cols = target_cols, group_col = NULL, split_cols = split_cols,
             format_to = format_to, drop_levels = drop_levels, add_levels_for_NA = add_levels_for_NA, blocks = blocks,
             indent_character = indent_character, indent = indent, ...)



      stopifnot(identical(tab_no_group[[atable::atable_options("colname_for_group")]],
                result[[atable::atable_options("colname_for_group")]]))

      tab_no_group <- doBy::renameCol(tab_no_group,
                                      atable::atable_options("colname_for_value"),
                                      atable::atable_options("colname_for_total")
      )

      if(!isTRUE(indent)){
        # indent_data_frame was not called. So number of columns are different. ID-variables are not all in the first columns
        tab_with_margin = merge(tab_no_group, result, sort=FALSE)
        class(tab_with_margin) <- class(result)
        tab_with_margin
      }else{




      tab_with_margin <- cbind(tab_no_group, result[-1])
      class(tab_with_margin) <- class(result) # the cbind does not know class "atable" and dispatches to data.frame.

      tab_with_margin
      }


    } else {result}

    return(result)
}


#' @export
#' @describeIn atable parses the formula and passes its parts to \code{atable}.
atable.formula <- function(formula, data, ...) {




    ff <- as.character(formula)

    LH <- ff[2]
    RH <- ff[3]

    pipe_split <- strsplit(RH, " \\| ")[[1]]  # as.character(f) always has blanks around those '|', which are not part of a name


    b <- !(all.vars(formula) %in% colnames(data))
    if (any(b)) {
        stop("Not all names of formula in colnames of data: ", paste(all.vars(formula)[b],
            collapse = ", "))
    }

    var_search <- function(x, data) {
        # Intersect of colnames(data) and x by grepl
        colnames_in_formula <- sapply(colnames(data), grepl, x = x, fixed = TRUE)  # order of names now as in data
        if (any(colnames_in_formula)) {
            colnames_in_formula <- names(which(colnames_in_formula))
            colnames_in_formula <- intersect(all.vars(formula), colnames_in_formula)  # order of names as given in the formula
            return(colnames_in_formula)
        } else {
            return(NULL)
        }
    }

    target_cols <- var_search(LH, data)
    group_col <- var_search(pipe_split[1], data)

    split_cols <- if (length(pipe_split) == 1) {
        NULL
    } else {
        if (length(pipe_split) > 2) {
            warning("formula should only have up to one '|' on the right side, not more. Taking only the first.")
        }
        var_search(pipe_split[2], data)
    }

    atable(x = data, target_cols = target_cols, group_col = group_col, split_cols = split_cols,
        ...)

}
