require(magrittr)

get_language <- function(file) {
  data <- jsonlite::fromJSON(file)

  language <- tibble::tibble(
    name = tolower(data$name),
    date_order = data$date_order,

    stop_words = list(
      tolower(unlist(data$stop_words)) %>%
        purrr::map_chr(get_search_variants)
    ),

    simplifications = list(
      unlist(data$simplifications)
    )
  )

  n <- unlist(purrr::map(data, length))

  replacements = list(
    tibble::tibble(
      before = tolower(unlist(data)),
      after = rep(names(data), n)
    ) %>%
    dplyr::filter(
      !(after %in% colnames(language))
    ) %>%
    dplyr::mutate(
      pattern = purrr::map_chr(
        before, get_search_variants
      )
    )
  )

  language$replacements <- replacements

  return(language)
}

get_search_variants <- function(x) {
  x <- stringr::str_split(x, " ")[[1]]

  variants <- stringr::str_sub(x, 1, 1) %>%
    sprintf("[%s|%s]", ., toupper(.)) %>%
    paste0(stringr::str_sub(x, 2))

  valid_chars <- "^\\p{L}" # unicode-friendly

  x <- paste(variants, collapse = " ") %>%
    sprintf("(?<=[%s]|^)%s(?=[%s]|$)",
      valid_chars, ., valid_chars)

  return(x)
}

languages <- list.files("./data-raw", ".json$",
    full.names = TRUE, ignore.case = TRUE) %>%
  purrr::map(get_language) %>% dplyr::bind_rows()

usethis::use_data(languages, overwrite = TRUE)
