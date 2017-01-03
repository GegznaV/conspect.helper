#' Insert R's code chunk for conspects
#'
#' Call this function as an addin to insert R's code chunk for
#'  conspects at the cursor position.
#'
#' @export
#'
#'
#' @family 'Insert at cursor position' addins
insert_chunk_r_Addin <- function() {
    TEXT =
        "



    ###


    ```{r, eval = F, include = F}

    ```

    ```{r}

    ```



    ***

    "
    rstudioapi::insertText(text = TEXT)
}

#' Insert python's code chunk for conspects
#'
#' Call this function as an addin to insert python's code chunk for conspects at the cursor position.
#'
#' @export
#'
#'
#' @family 'Insert at cursor position' addins
insert_chunk_python_Addin <- function() {
    TEXT =
        "



    ###


    ```{python, eval = F, include = F}

    ```

    ```{python}

    ```



    ***

    "
    rstudioapi::insertText(text = TEXT)
}


# =============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' [!] Correnct md text genereted with Writage
#'
#'
#' @param x A sting.
#'
#' @return A corrected string.
#' @export
#' @examples
#'
#'     pattern     <- c( "\\`", "\\]",   "\\[",    "\\_",  "\\\\" )
#'     replacement <- c( "`",     "]",     "[",      "_",  "\\" )

correct_writage_md <- function(TEXT){
    # filename <- "D:/_Kursai/DataCamp/Intro Statistics with R (Module 1, 2)/R (IntroStat R)/R (IntroStat R) - Copy.md"
    #
    # TEXT <- readLines(filename, encoding = "UTF-8")

    # Remove unnecessay slashes
    pattern     <- c( "\\`", "\\]", "\\[",  "\\_","\\*", "\\\\", " ", "&lt;", "&gt;" )
    replacement <- c( "`",     "]",   "[",    "_", "*",  "\\",  " ", "<", ">" )

    i <- 1

    for (i in 1:length(pattern)){
        TEXT <- gsub(pattern     = pattern[i],
                     replacement = replacement[i],
                     x = TEXT,
                     fixed = TRUE)
    }

    # Remove empty lines
    # TEXT[TEXT != ""]

    return(TEXT)
}


#' [!] Correnct md text genereted with Writage (add-in)
#'
#' @export
#' @family 'Replace selected symbols' addins

correct_writage_md_addin <- function(){
    context <- rstudioapi::getActiveDocumentContext()
    #
    # '\\'

    for (sel in context$selection) {
        TXT  <- sel$text
        Encoding(TXT) <- "UTF-8"

        new_TXT <- correct_writage_md(TXT)

        rstudioapi::modifyRange(sel$range, as.character(new_TXT), context$id)
        break
    }
}

