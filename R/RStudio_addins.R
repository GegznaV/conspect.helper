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
#'     pattern     <- c( "\\`", "\\]",   "\\[",    "\\_",  "\\\\" , "\\#")
#'     replacement <- c( "`",     "]",     "[",      "_",  "\\" , "#")

correct_writage_md <- function(TEXT){
    # filename <- "D:/_Kursai/DataCamp/Intro Statistics with R (Module 1, 2)/R (IntroStat R)/R (IntroStat R) - Copy.md"
    #
    # TEXT <- readLines(filename, encoding = "UTF-8")

    # Remove unnecessay slashes
    pattern     <- c( "\\`", "\\]", "\\[",  "\\_","\\*", "\\\\", " ", "&lt;", "&gt;", "\\#", "<sup>","</sup>", "<sub>", "</sub>" )
    replacement <- c( "`",     "]",   "[",    "_", "*",  "\\",  " ", "<", ">", "#", "^","^", "~", "~")

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





#' Enclose with R code chunk
#'
#' Call this function as an addin to enclose text into R code chunk
#'
#' @export
#'
#'
#' @family 'Enclose with' addins
enclose_with_r_chunk_Addin <- function() {
    # spAddins:::enclose_selection_with("```{r}\n", "\n```")}
    addin.tools:::rs_enclose_selection_with("```{r}\n", "\n```")}

# Name: Enclose with R code chunk
# Description: Enclose text with R code chunk in knitr document
# Binding: enclose_with_r_chunk_Addin
# Interactive: false
# =============================================================================

block_quotes_to_r_chunk <- function(text_){
    txt2 <- paste0("\n", text_, "\n")  %>%
        textConnection()  %>%
        readLines()

    has_quote <- grepl(pattern = "^> ", txt2)

    txt2 <- sub(pattern = "^> ", "", txt2)
    status <- c(0, diff(has_quote))

    for (i in seq_along(status)) {
        if (status[i] == 1) {
            txt2[i] = paste0("\n```{r}\n", txt2[i])
        } else if (status[i] == -1) {
            txt2[i] = paste0("```\n", txt2[i])
        }
    }

    paste0(txt2, collapse = "\n")
}

# block_quuotes_to_r_chunk(text)  %>% cat()

#' [!] Correnct md text genereted with Writage (add-in): block quote to R chunk
#'
#' @export
#'
#'
#' @family 'Replace selected symbols' addins
correct_block2rchunk_Addin <- function(){
    context <- rstudioapi::getActiveDocumentContext()
    #
    # '\\'

    for (sel in context$selection) {
        TXT  <- sel$text
        Encoding(TXT) <- "UTF-8"

        new_TXT <- block_quotes_to_r_chunk(TXT)

        rstudioapi::modifyRange(sel$range, as.character(new_TXT), context$id)
        break
    }
}
# ============================================================================

#' @export
add_to_html_head <- function(filename,
                             line_to_add,
                             locale = readr::default_locale()){

    # Sys.setlocale(locale = locale)

    # txt2 <- readLines(filename, encoding = "UTF-8")
    # txt2 <- enc2utf8(txt2)
    # cat(txt2, file = filename, sep = "\n")
    # writeLines(txt2, filename, useBytes = TRUE)

    library(magrittr)

    txt1 <- readr::read_lines(filename, locale = locale)

    if (any(grepl(line_to_add, txt1))) {
        message("The file already contains the `line_to_add`.")
        txt2 <- txt1
    } else {
        txt2 <- sub(pattern = "(</head>)",
            replacement = paste0("  ", line_to_add, "\n\\1"),
            x = txt1)
    }



    if (!identical(txt1, txt2)) {
        readr::write_lines(txt2, path = filename)
        message("File updated.")
    } else {
        message("File was not changed.")
    }

}
#
# line_to_add <- '<link rel="icon" href="https://gegznav.github.io/pratybos/favicon.png" />'
#
# files <- dir("_book", pattern = ".html$")
#
#







