% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_lines.R
\name{convert_to_lines}
\alias{convert_to_lines}
\title{Converts text into separate lines}
\usage{
convert_to_lines(x)
}
\arguments{
\item{x}{(character) Either a path to a file, a connection,
or literal data (either a single string or a raw vector).\cr
Literal data is most useful for examples and tests.
It must contain at least one new line to be recognised as data
(instead of a path).}
}
\value{
A vector of strings.
}
\description{
Converts text into separate lines
}
\examples{

convert_to_lines("a\nb")

# expect error, as "a" is interpreted as a file name:

# convert_to_lines("a")
}
