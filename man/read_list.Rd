% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_list.R
\name{read_list}
\alias{read_list}
\title{Read R list, printed to a text file}
\usage{
read_list(x)
}
\arguments{
\item{x}{(character) Either a path to a file, a connection,
or literal data (either a single string or a raw vector).\cr
Literal data is most useful for examples and tests.
It must contain at least one new line to be recognised as data
(instead of a path).}
}
\value{
An R list.
}
\description{
Function reads a simple list, which contain vectors and functions.
}
\examples{
library(conspect.helper)


FILE <- "data-raw/my_control_04.txt"
read_list(FILE)


read_list(
'$method
[1] "cv"

$number
[1] 10

$repeats
[1] 1
')


}
