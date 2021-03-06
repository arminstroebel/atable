#' A data.frame with standardized random data of various classes
#'
#' A data.frame intended for testing the atable function with standardized random data and missing values in various classes.
#'
#'  For every subset defined by a triplet of the levels of Split1, Split2 and Group the variables have the following properties:
#'  \itemize{
#'    \item 60 observations
#'    \item Logical has exactly the same number of \code{TRUE} and \code{FALSE} and \code{NA} (20).
#'    \item Factor has exactly the same number of levels taken and \code{NA} (15).
#'    \item Ordered has exactly the same number of levels taken  and \code{NA} (12).
#'    \item Numeric is sampled from a normal distribution and then standardized to
#'      \code{\link[stats]{sd}} 1 and with 6 \code{NA}.
#'      Its \code{\link[base]{mean}} is 12 when \code{Group} is \code{'Treatment'}
#'      and 10 otherwise (up to \code{10^-17}).
#' }
#'
#'
#' @format A data frame with 1080 rows and 7 variables:
#' \describe{
#'   \item{Split1}{A factor with 2 levels without \code{NA}. The two levels have the same frequency (540).}
#'   \item{Split2}{A factor with 2 levels with \code{NA}. The two levels and the \code{NA} have the same frequency (360).}
#'   \item{Group}{A factor with 2 levels with \code{NA}. The two levels and the \code{NA} have the same frequency (360).}
#'   \item{Logical}{A logical.}
#'   \item{Factor}{A factor with 3 levels.}
#'   \item{Ordered}{Class ordered with 4 levels.}
#'   \item{Numeric}{Class numeric.}
#'
#' }
#'
#' @examples
#' atable::atable(Logical+ Numeric + Factor + Ordered ~ Group | Split1 + Split2,
#'  atable::standardized_test_data, add_levels_for_NA = TRUE, format_to = 'Word')
#'
"standardized_test_data"
