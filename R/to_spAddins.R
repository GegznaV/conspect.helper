#' Insert Lithuanian quotes (lower)
#'
#' Call this function as an addin to insert lower Lithuanian quotes.
#'
#' @export
#'
#'
#' @family 'Insert at cursor position' addins
insert_lt_quotes_low_Addin <- function() {
    TEXT = "„"
    rstudioapi::insertText(text = TEXT)
}

#' Insert Lithuanian quotes (upper)
#'
#' Call this function as an addin to insert upper Lithuanian quotes.
#'
#' @export
#'
#'
#' @family 'Insert at cursor position' addins
insert_lt_quotes_upp_Addin <- function() {
    TEXT = "“"
    rstudioapi::insertText(text = TEXT)
}