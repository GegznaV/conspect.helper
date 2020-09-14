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


# Name: Insert LT quotes (upper)
# Description: Insert LT quotes (upper)
# Binding: insert_lt_quotes_upp_Addin
# Interactive: false
#
# Name: Insert LT quotes (lower)
# Description: Insert LT quotes (lower)
# Binding: insert_lt_quotes_low_Addin
# Interactive: false