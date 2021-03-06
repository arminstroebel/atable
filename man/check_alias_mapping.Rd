% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_alias_mapping.R
\name{check_alias_mapping}
\alias{check_alias_mapping}
\title{Checks the output of function create_alias_mapping}
\usage{
check_alias_mapping(Alias_mapping)
}
\arguments{
\item{Alias_mapping}{Result of function \code{create_alias_mapping}.}
}
\value{
\code{TRUE} if \code{x} has the following properties:
\code{Alias_mapping} is a non-empty data.frame with character columns \code{'old'} and \code{'new'}, without NA and "".
Column \code{'new'} has no duplicates.
Else throws an error. Prints the duplicates of column \code{'new'}, if available.
}
\description{
Checks the output of function \code{\link{create_alias_mapping}}.
}
