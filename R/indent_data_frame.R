#' Indents data.frames
#'
#' Indents data.frames for printing them as tables.
#'
#' Squeeze multiple key-columns into one column and indents the values accordingly.
#' Adds new lines with the indented keys to the data.frame.
#' Meant for wide tables that need to be narrower and more 'readable'
#' Meant for plotting with e.g. xtable::xtable or Hmisc::latex or officer::body_add_table.
#' Look at the examples for a more precise description.
#' Meant for left-aligned columns. Thats why the \code{indent_character} is inserted
#' to the left of the original values.
#'
#'
#'
#' @param DD A data.frame. Should be sorted by \code{keys} with \code{keys[1]} varying slowest and \code{keys[length(keys)]} varying fastest.
#' @param keys A character. Subset of \code{colnames(DD)} with \code{length(keys)>=2}. The combination of keys must be unique. \code{DD[keys]} must be class character or factor.
#' @param values A character. Subset of colnames(DD). DD[keys] must be class character, factor or numeric.
#' @param character_empty A character. Default ''. This character will be put in the new lines in class character columns.
#' @param numeric_empty A numeric. Default NA.  This character will be put in the new lines in class numeric columns.
#' @param indent_character A character. character for one indent. Default is '\\quad' (meant for latex). Can also be '   ' for Word.
#' @param colname_indent A character. Default 'Group'. Name of the new column with the indented keys.
#'
#' @return A data.frame. Columns: \code{c(colname_indent, values)}.
#' Column \code{colname_indent} contains all combination of \code{DD[keys]}, but now indented and squeezed in this column and casted to character.
#' Columns \code{'values'} contain all values of \code{DD[values]} unchanged.
#' Number of rows is \code{sum(cumprod(nlevels(DD[keys])))}.
#'
#' @examples
#'
#'DD <- expand.grid(Arm = paste0('Arm ', c(1,2,4)),
#'                 Gender = c('Male', 'Female'),
#'                 Haircolor = c('Red', 'Green', 'Blue'),
#'                 Income = c('Low', 'Med', 'High'), stringsAsFactors = TRUE)
#'
#' DD <- doBy::orderBy(~ Arm + Gender + Haircolor + Income, DD)
#'
#' DD$values1 <- runif(dim(DD)[1])
#' DD$values2 <- 1
#' DD$values3 <- sample(letters[1:4], size = nrow(DD), replace = TRUE)
#'
#' keys = c('Arm', 'Gender', 'Haircolor', 'Income')
#' values = c('values1', 'values2', 'values3')
#' \dontrun{
#' DDD <- indent_data_frame(DD, keys, indent_character = '   ')
#'
#' # print both:
#'
#' Hmisc::latex(DD,
#'       file = '',
#'       longtable = TRUE,
#'       caption = 'Original table',
#'       rowname = NULL)
#'
#' Hmisc::latex(DDD,
#'       file = '',
#'       longtable = TRUE,
#'       caption = 'Indented table',
#'       rowname = NULL)
#'       }

indent_data_frame <- function(DD, keys, values = setdiff(colnames(DD), keys), character_empty = "",
    numeric_empty = NA, indent_character = "\\quad", colname_indent = "Group") {

    stopifnot(is.data.frame(DD), is.character(keys), is.character(values), keys %in%
        colnames(DD), values %in% colnames(DD), anyDuplicated(c(keys, values, colname_indent)) ==
        0)


    # at least two keys. Otherwise no indent necessary
    stopifnot(length(keys) >= 2)


    # keys must be character or factor stopifnot( sapply(DD[keys], is.character) |
    # sapply(DD[keys], is.factor))

    # DD must be ordered by keys: keys[1] must vary slowest, keys[length(keys)] must
    # vary fastest

    ff <- paste("~", paste(keys, collapse = "+"), collapse = "")

    DD_ordered <- doBy::orderBy(stats::as.formula(ff), DD)

    b <- all.equal(DD_ordered, DD, check.attributes = FALSE)

    DD <- if (!isTRUE(b)) {
        warning("DD is not ordered by keys. Reordering...")
        DD_ordered
    } else {
        DD
    }





    # line_adder

    line_adder <- function(DD, keys, values, the_key, character_empty = "", numeric_empty = NA) {
        # adds a new line on top of the data frame with keys, value are empty.

        to_add <- DD[1, , drop = FALSE]

        # empty columns empty key columns
        empty_keys <- setdiff(keys, keys[1:which(keys == the_key)])
        to_add[empty_keys] <- character_empty

        # empty value columns

        return_empty_value <- function(x) {
            type <- class(x)
            switch(type, character = character_empty, numeric = numeric_empty, integer = numeric_empty,
                factor = character_empty)
        }

        to_add[values] <- lapply(to_add[values], return_empty_value)


        return(rbind(to_add, DD))
    }



    # Apply line_adder for every key combination, Order: backwards

    for (index in seq(from = length(keys) - 1, to = 1, by = -1)) {
        DD <- plyr::ddply(DD, keys[1:index], line_adder, keys = keys, values = values,
            the_key = keys[index])
    }

    # casts everything to character and then replace_consecutive
    DD[keys] <- lapply(DD[keys], as.character)
    DD[keys] <- lapply(DD[keys], replace_consecutive, by = indent_character)


    # indent

    indent <- DD[keys]  # keys has length 2 and no duplicates. So DD[keys] is a data.frame, even witout drop=FALSE for '['
    indent <- apply(indent, 1, paste, collapse = " ")



    DD[[colname_indent]] <- indent

    DD <- DD[c(colname_indent, values)]

    return(DD)
}
