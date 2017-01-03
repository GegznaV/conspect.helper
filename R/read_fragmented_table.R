#' Read fragmented table from a flat file.
#'
#'  A convenience function, that wraps \code{read_tabe}
#'  from package \pkg{readr}.
#'
#' @param FILE (string) a file name.
#'
#' @return a data table.
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' # Contents of file "data-raw/CO_head.txt":
#'
#' #   Plant   Type  Treatment
#' # 1   Qn1 Quebec nonchilled
#' # 2   Qn1 Quebec nonchilled
#' # 3   Qn1 Quebec nonchilled
#' #   conc uptake
#' # 1   95   16.0
#' # 2  175   30.4
#' # 3  250   34.8
#'
#'
#' read_fragmented_table("data-raw/CO_head.txt")
#'
#' ##   Plant   Type  Treatment conc uptake
#' ## 1   Qn1 Quebec nonchilled   95   16.0
#' ## 2   Qn1 Quebec nonchilled  175   30.4
#' ## 3   Qn1 Quebec nonchilled  250   34.8
#' ##
#' ## Warning messages:
#' ##     1: Missing column names filled in: 'X1' [1]
#' ##     2: Missing column names filled in: 'X1' [1]
#'
#' # These warning messages are expected.
#'
#' }}
#'
#' @import magrittr
read_fragmented_table <- function(FILE){

    # require(readr)
    # require(dplyr)

    LINES <- readLines(FILE)
    n_total <- LINES %>% length()

    # METHOD 1: number of lines plus header:
    # (result is correct if row numbers start at 1 an and are sequential)
    n_unique1 <- utils::read.table(file = FILE, skip = n_total-1, header = F)$V1 + 1

    # METHOD 2: If first character is a space, the like is treated as header line
    # (result is correct if table's format is appropriate)
    repetitions2 <- (substr(LINES, 1, 2) == "  ") %>% sum()


    repetitions1 <- n_total / n_unique1     # method 1
    n_unique2    <- n_total / repetitions2  # method 2

    if(repetitions1 %% 1 != 0 | n_unique2 %% 1 != 0) {
        warning("Number of repetitions calculated by various methods does not match.")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (repetitions1 %% 1 == 0) {
        repetitions <- repetitions1
        n_unique    <- n_unique1

    } else if (n_unique2 %% 1 == 0) {
        repetitions <- repetitions2
        n_unique    <- n_unique2

    } else  {
        stop("Result is incorrect:",
             "\nNumber of repetitions must be integer: ",   repetitions1,
             "\nNumber of unique values must be integer: ",  n_unique2)

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    n_skip <- ((1:repetitions)-1)* n_unique

    read_parts <- function(n_skip_i) {
        readr::read_table(FILE,
                   col_names = TRUE,
                   skip   = n_skip_i,
                   n_max  = n_unique)  }

    data_list <- lapply(n_skip, read_parts)

    Reduce(merge.data.frame, data_list)  %>%
        dplyr::select(-X1)
}
