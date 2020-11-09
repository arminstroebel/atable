#' A wrapper for atable
#'
#' Set parameters for more compact tables.
#' Arguments are passed to \code{\link{atable}}. See the help for there.
#'
#' @param x object passed to atable.
#'



atable_compact = function(x,
                          format_statistics.statistics_factor = atable_options("format_statistics_compact.statistics_factor"),
                          format_statistics.statistics_numeric = atable_options("format_statistics_compact.statistics_numeric"),
                          ...)
{

  atable(x,
         indent = FALSE,
         format_statistics.statistics_factor = format_statistics.statistics_factor,
         format_statistics.statistics_numeric = format_statistics.statistics_numeric,
         ...)
}
