
#' Converts text into separate lines
#'
#' @param x (character) Either a path to a file, a connection,
#' or literal data (either a single string or a raw vector).\cr
#' Literal data is most useful for examples and tests.
#' It must contain at least one new line to be recognised as data
#' (instead of a path).
#'
#' @return A vector of strings.
#' @export
#'
#' @examples
#'
#' convert_to_lines("a\nb")
#'
#' # expect error, as "a" is interpreted as a file name:
#'
#' # convert_to_lines("a")

convert_to_lines <- function(x){

    if (is.character(x)) {
        if (grepl("\n", x)) {
            # "literal data"
            textLines <- base::strsplit(x, "\n", fixed = TRUE) %>%
                unlist()
        }
        else {
            textLines <- readLines(x, encoding = "UTF-8")
        }
    } else if (inherits(x, "connection")) {
        textLines <- readLines(x, encoding = "UTF-8")
    }

    textLines
}