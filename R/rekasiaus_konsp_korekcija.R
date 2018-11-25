# =============================================================================
#' @export
rekasius2rmd <- function(text_ = NULL){
    library(magrittr)
    library(stringr)
    library(stringi)
    Sys.setlocale(locale = "Lithuanian")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    to_title_lt <-
        function(txt) {
            stri_trans_totitle(txt,
                               opts_brkiter = stri_opts_brkiter(locale = "lt",
                                                                type = "sentence"))
        }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    txt1 <- paste0("\n", text_,"\n")  %>%
        textConnection()  %>%
        readLines(encoding = "ISO-8859-1")


    # folder <-
    #     'D:/Dokumentai/R/pratybos-biostatistika_2018/_resursai/Rkonspektas--T-Rekasius/'
    #
    # txt1 <- readLines(
    #     fullfile(folder, "1.1_vektoriu_sudarymas_enc.R"),
    #     # fullfile(folder, "7.2_rand_generatorius.R"),
    #     encoding = "ISO-8859-1")

    ind_head_end <- (which(str_detect(txt1, "TURINYS")) - 2)

    # Head
    txt_h <- txt1[1:ind_head_end]

    # Body
    txt_b <- txt1[(ind_head_end + 1):length(txt1)]

    # Process head -----------------------------------------------------------
    epmty_rows <- str_detect(txt_h, "^(#)?( )*$")

    txt_yaml <-
        (txt_h[!epmty_rows]) %>%
        str_replace("^#( )*+([^\\:]*)\\.$",  'title: "\\2"') %>%
        str_replace("^#( )*+(Dalykas:.*)$",  to_title_lt) %>%
        str_replace("^#( )*+(Dalykas:.*)$",  'subtitle: "\\2"') %>%
        str_replace("^#( )*+(Autorius:.*)$", 'author: "\\2"') %>%
        str_replace("^#( )*+(Sukurta:.*?)[ ]*\\|[ ]*(.*?)$",  'date: "\\2, atnaujinta: \\3"') %>%
        str_replace("^#( )*+(.*)$", " \\2") %>%
        c("---", ., "---")


    # Process body -----------------------------------------------------------

    # wo_hashtag <- (grepl(pattern = "^[^# ]", txt2) & txt2 != "")

    wo_hashtag <- (str_detect(txt_b, pattern = "^[^# ]") & txt_b != "")

    status <- c(0, diff(wo_hashtag))

    txt_b2 <- txt_b %>%
        # styler::style_text() %>%
        str_replace_all("^# -* #$",        replacement = "") %>%
        str_replace_all("(^# .*) #$",      replacement = to_title_lt) %>%
        str_replace_all("(^# .*) #$",      replacement = "# \\1") %>%
        str_replace_all("(^# .*?) ?-{3,}", replacement = to_title_lt) %>%
        str_replace_all("(^# .*?) ?-{3,}", replacement = "# #\\1") %>%
        str_replace_all("(^# ?)",          replacement = "")
        # str_replace_all("^( )*(Sukurta: .*)$", replacement = "<!-- \\1 -->") %>%
        # str_replace_all("^( )*(Sukurta: .*)$", replacement = "<!-- \\1 -->") %>%
        # str_replace_all("^( )*(Sukurta: .*)$", replacement = "<!-- \\1 -->")

        # sub(pattern = "^# -* #$", "", .)        %>%
        # sub(pattern = "(^# .*) #$", "# \\1", .)  %>%
        # sub(pattern = "(^# .*?) ?-{3,}", "# #\\1", .)  %>%
        # sub(pattern = "(^# ?)", "", .)


    # make R code cunks
    for (i in seq_along(status)) {
        if (status[i] == 1) {
            # txt_b2[i] = paste0("\n```{r}\n", txt_b2[i])
            txt_b2[i] = paste0("\n```{r}\n", txt_b2[i])
        } else if (status[i] == -1) {
            txt_b2[i] = paste0("```\n", txt_b2[i])
        }
    }


    paste0(c(txt_yaml, txt_b2), collapse = "\n")
    # writeLines(text_, txt2)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
rekasius2rmd2 <- function(text_){
    require(magrittr)
    Sys.setlocale(locale = "Lithuanian")

    # txt2 <- paste0("\n", text_,"\n")  %>%
    #     textConnection()  %>%
    #     readLines()

    txt2 <- readLines(text_, encoding = "UTF-8")

    wo_hashtag <- (grepl(pattern = "^[^# ]", txt2) & txt2 != "")
    status <- c(0, diff(wo_hashtag))
    txt2 %<>%
        sub(pattern = "^# -* #$", "",.)        %>%
        sub(pattern = "(^# .*) #$", "# \\1", .)  %>%
        sub(pattern = "(^# .*?) ?-{3,}", "# #\\1", .)  %>%
        sub(pattern = "(^# ?)", "", .)


    # txt_head <- txt2[1:10]
    #
    #
    # sub("( *)Dalykas: (.*)","Title: \\2", txt_head)
    #
    # make R code cunks
    for (i in seq_along(status)){
        if (status[i] == 1){
            txt2[i] = paste0("\n```{r}\n", txt2[i])
        } else if (status[i] == -1) {
            txt2[i] = paste0("```\n", txt2[i])
        }
    }


    cat(paste0(txt2, collapse = "\n"), file = text_ )
    # writeLines(txt2, text_)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' [!] Correnct md text genereted with Writage (add-in): block quote to R chunk
#'
#' @export
#'
#'
#' @family 'Replace selected symbols' addins
rekasius2rmd_Addin <- function(){
    context <- rstudioapi::getActiveDocumentContext()
    #
    # '\\'

    for (sel in context$selection) {
        TXT  <- sel$text
        Encoding(TXT) <- "UTF-8"

        new_TXT <- rekasius2rmd(TXT)

        rstudioapi::modifyRange(sel$range, as.character(new_TXT), context$id)
        break
    }
}
