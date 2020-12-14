#' More compact formatting than atable()
#'
#' This is a wrapper for atable(), calculating the same statistics, but with different format.
#'
#' The compact formatting is:
#'
#' Numeric target_cols get one line in the table; the line contains the mean and SD of the variable.
#'
#' Factor target_cols also get one line in the table, when they have only two levels and only the first level
#' is displayed in the table and the name of the variable is omitted. This is intended for item like "Sex at birth: Female/Male".
#' Knowing the percentage of Female is sufficient in this case (when NAs are not counted).
#' Be careful with items like "Pregnant: Yes/No".
#' Here only the level "Yes" will be printed and the name of the item (Pregnant) is omitted, making the table uninformative.
#' Factors with three or more levels get one line per level, the levels are intended and a header line
#' containing the name of the variable is added.
#'
#' Arguments in ... are passed to \code{\link{atable}}. See the help there.
#' \code{atable_compact} is not designed for splitted atables, so argument \code{split_cols} must be omitted or NULL.
#' Also argument \code{format_to} is ignored.
#' Other features of atable (blocking, add_margins, alias) are available, see examples.
#'
#' @param x object passed to atable.
#' @param target_cols character. Some of colnames(x).
#' @param group_col character or NULL. If character then, one of colnames(x).
#' @param indent_character character length 1. Default is defined in table_options("indent_character_compact").
#' For Latex-Format use e.g. \code{indent_character="\\quad"}.
#' For Word-Format use e.g. \code{indent_character=paste0(rep(intToUtf8(160), 5), collapse = "")} and e.g.
#' Package officer and its functions officer::read_docx(), officer::body_add_table and print-methods.
#' @param blocks NULL or a list, passed to atable, see help there.
#' @param format_factor a function that defines the format of factor variables.
#' Default is defined in \code{\link{atable_options}}. See \code{\link{check_format_statistics}} for the return-value of this function.
#' @param format_numeric a function that defines the format of numeric variables.
#' Analog to format_factor.
#'
#' @param ... Passed to \code{\link{atable}}.
#'
#' @return data.frame
#'
#'@examples
#' # For Console:
#' atable_compact(
#'   atable::test_data,
#'   target_cols = c("Numeric", "Numeric2", "Split2", "Factor", "Ordered"),
#'   group_col = "Group2",
#'   blocks = list("Primary Endpoint" = "Numeric",
#'                 "Secondary Endpoints" = c("Numeric2", "Split2", "Factor")),
#'   add_margins = TRUE)
#'
#' # The target_cols are "Numeric", "Numeric2", "Split2", "Factor", "Ordered".
#' # The group_col is "Group2".
#' # The data.frame is grouped by group_col and the summary statistcs of the target_cols are
#' # calculated: mean, sd for numeric, counts and percentages for factors.
#' # Some target_cols are blocked: the first block 'Primary Endpoint' contains the variable Numeric.
#' # The second block 'Secondary  Endpoint' contains the variables "Numeric2", "Split2", "Factor".
#' # The blocks are intended.
#' # For variable Split2 only the first level is reported, as the variable has only two levels and
#' # the name 'Split2' does not appear in the table.
#' # The variable Factor has more than two levels, so all of them are
#' # reported and appropriately intended.
#' # The variable Ordered is not part of a block and thus not intended.
#'
#' # For Latex:
#' # Same as for Console, but with different indent_character:
#'
#' tab = atable_compact(atable::test_data,
#'                      target_cols = c("Numeric", "Numeric2", "Logical", "Factor", "Ordered"),
#'                      group_col = "Group2",
#'                      indent_character = "\\quad")
#'
#' tab = atable::translate_to_LaTeX(tab)
#'
#' # Then call e.g. Hmisc::latex(tab, ...)
#'
#'
#' # Example for Word format:
#' \dontrun{
#' tab = atable_compact(
#'   atable::test_data,
#'   target_cols = c("Numeric", "Numeric2", "Split2", "Factor", "Ordered", "Character"),
#'   group_col = "Group2",
#'   blocks = list("Primary Endpoint" = "Numeric",
#'                 "Secondary Endpoints" = c("Numeric2", "Split2", "Factor")),
#'   add_margins = TRUE,
#'   indent_character = paste0(rep(intToUtf8(160), 5), collapse = ""))
#'
#' # The argument indent_character has the value intToUtf8(160) (non breakable space).
#' # This is the important part:
#' # Spaces at the beginning of a cell of a data.frame are somehow lost on the way to the docx.
#' # Other indent_characters may also do the job.
#'
#' doc = officer::read_docx()
#' doc = officer::body_add_table(doc,tab)
#'
#' print(doc, target = "atable_Word.docx")
#'
#' # Other packages may exist for Word-export.
#' }
#'
#'



#' @export
atable_compact = function(x, ...)
{
  UseMethod("atable_compact")
}

#' @export
#' @describeIn atable_compact a compact version of atable.
atable_compact.data.frame = function(x,
                                     target_cols,
                                     group_col = NULL,
                                     indent_character = atable_options("indent_character_compact"),
                                     blocks = NULL,
                                     format_factor = atable_options("format_statistics_compact.statistics_factor"),
                                     format_numeric = atable_options("format_statistics_compact.statistics_numeric"),
                                     ...)
{

  # atable no indent
  tab <- atable(x = x,
                target_cols = target_cols,
                group_col = group_col,
                split_cols = NULL,
                indent = FALSE,
                blocks = blocks,
                indent_character = indent_character,
                format_statistics.statistics_factor = format_factor,
                format_statistics.statistics_numeric = format_numeric,
                ...)



  # alias mapping
  Alias_mapping <- create_alias_mapping(x[c(target_cols)])

  # map target_cols to aliases
  tab[[atable_options("colname_for_variable")]] <- plyr::mapvalues(x = tab[[atable_options("colname_for_variable")]],
                                                                   from = Alias_mapping$old, to = Alias_mapping$new, warn_missing = FALSE)



  # map blocks by alias if blocks available
  blocks <- if(!is.null(blocks)){
    lapply(blocks, plyr::mapvalues,
           from = Alias_mapping$old,
           to = Alias_mapping$new,
           warn_missing = FALSE)}
  else{blocks}


  name_adder <- function(x, name){data.frame(target_cols = x,
                                             block_name = name,
                                             stringsAsFactors = FALSE)}


  bb <- if(!is.null(blocks)){
    # format blocks from list to data.frame
    bb <- mapply(name_adder, x=blocks, name=names(blocks),
                SIMPLIFY = FALSE)

    bb <- do.call(rbind, bb)

    bb <- doBy::renameCol(bb, "target_cols", atable_options("colname_for_variable"))

    bb$block_name <- factor(bb$block_name, levels = names(blocks)) # the order of the blocks is necessary

    bb <- doBy::renameCol(bb, "block_name", atable_options("colname_for_blocks"))} else {NULL}





  tab <- if(!is.null(blocks)){
    # add blocks as a extra column
    # base::merge.data.frame sorts the data.frame, even with argument sort=FALSE.
    # So I use plyr::join
    plyr::join(tab, bb,
               type = "left",
               by = atable_options("colname_for_variable"))} else {tab}


  # indent of tag and value column
  tab <- plyr::ddply(.data = tab,
                     .variables = atable_options("colname_for_variable"),
                     .fun = indent_tag_value,
                     indent_character = indent_character)

  tab <- if(!is.null(blocks)){
    # move blocks-column first
    tab <- tab[c(atable_options("colname_for_blocks"), setdiff(colnames(tab), atable_options("colname_for_blocks")))]

    indent_blocks(tab, indent_character)

  }else{tab}


  # rename colname_for_variable (first column)
  tab <- doBy::renameCol(tab,
                         atable_options("colname_for_variable"),
                         atable_options("colname_for_variable_compact"))


  # class for printing in console
  class(tab) <- c("atable", "data.frame")
  tab[is.na(tab)] <- ""

  rownames(tab) <- NULL

  return(tab)

}
