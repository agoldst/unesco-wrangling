
library(dplyr)
library(rvest)
library(readr)
library(stringr)

extract_frame <- function (data_dir) {
    extract <- function (b, key) {
        e <- b %>% html_nodes(key) %>% html_text()
        if (length(e) == 0) {
            NA
        } else {
            str_c(e, collapse=";;")
        }
    }

    extract_names <- function (b, role) {
        spans <- html_nodes(b, "span")
        i_last <- which(html_attr(spans, "class") ==
                        str_c("sn_", role, "_name"))
        if (length(i_last) == 0) {
            return(NA)
        }
        firsts <- ifelse(
            html_attr(
                spans[i_last + 1], "class") ==
                    str_c("sn_", role, "_firstname"),
                str_c(", ", html_text(spans[i_last + 1])),
                ""
            )
        str_c(html_text(spans[i_last]), firsts, collapse=";;")
    }


    proc_file <- function (f) {
        doc <- html(f, encoding="utf8")
        data_frame(
            id=doc %>%
                html_nodes("td.res1") %>%
                html_text(),
            b=doc %>% html_nodes("td.res2")
        ) %>%
            mutate(id=str_replace(id, "/.*$", "")) %>%
            group_by(id) %>%
            do({
                data_frame(
                    author=.$b %>% extract_names("auth"),
                    title=.$b %>% extract(".sn_target_title"),
                    translator=.$b %>% extract_names("transl"),
                    year=.$b %>% extract(".sn_year"),
                    publisher=.$b %>% extract(".publisher"),
                    pub_place=.$b %>% extract(".place")
                )
            })
    }

    fs <- Sys.glob(file.path(data_dir, "*.html")) 

    b_list <- vector("list", length(fs))

    p <- progress_estimated(length(fs))

    for (i in seq_along(fs)) {
        b_list[[i]] <- proc_file(fs[i])
        p$tick()$print()
    }

    bind_rows(b_list)
}

