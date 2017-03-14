#' Read R list, printed to a text file
#'
#' Function reads a simple list, which contain vectors and functions.
#'
#'
#' @inheritParams convert_to_lines
#'
#' @return An R list.
#' @export
#'
#' @examples
#' library(conspect.helper)
#'
#'
#' FILE <- "data-raw/my_control_04.txt"
#' read_list(FILE)
#'
#'
#' read_list(
#' '$method
#' [1] "cv"
#'
#' $number
#' [1] 10
#'
#' $repeats
#' [1] 1
#' ')
#'
#'
#' @import spMisc
#'
read_list <- function(x){

    textLines <- convert_to_lines(x)

    # Filter out unnecesarry information and rows
    as.lines <- function(df) df$x
    textLines %<>%
        sub("^\\[.*\\] (.*)","\\1", .)  %>% # remove row names in brackets, e.g., [1]
        sub("(^<.*>)","# \\1",. )  %>% # detect functions
        dplyr::data_frame(x = .)  %>%
        dplyr::filter(x != "")  %>%
        as.lines()

    # Define name of a list
    obj_name <- sub("^# (.*)","\\1", textLines[1]) %>% make.names()
    eval_(obj_name  %++% " <- list()")

    # Extract variables
    starts <- grepl("^\\$", textLines)  %>% which()
    ends   <- c(starts[-1]-1, length(textLines))


    # Parse the structure
    for (i in seq_along(starts)){

        S_i <- starts[i]
        E_i <- ends[i]

        if (S_i == E_i) {
            message(i, "/", length(starts), " element skipped: ", textLines[S_i])
            next()
        }

        var_name <- obj_name %++% textLines[S_i]

        is_function <- grepl("(^# <.*>)", textLines[E_i])
        has_several_columns <- grepl(" ", textLines[S_i+1])

        i_rows <- (S_i+1):E_i
        n_rows <- length(i_rows)

        current_rows <- textLines[i_rows]

        if (is_function) {
            var_contents <- paste(current_rows, collapse = "\n")
            eval_(var_name  %++% " <- " %++% var_contents)

        } else if (has_several_columns & n_rows > 1) {
            var_contents <- read.table(text = c(current_rows))
            warning("Possibly incorrect result in: \n", var_name)

            eval_(var_name  %++% " <- var_contents")

        } else if (has_several_columns & n_rows == 1) {
            var_contents <- read.table(text = c(current_rows)) %>%
                as.matrix()  %>%
                as.vector()
            eval_(var_name  %++% " <- var_contents")

        } else {
            eval_(var_name  %++% " <- " %++% current_rows)
        }
    }
    # Rezult
    eval_(obj_name)
}



