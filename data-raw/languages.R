require(magrittr)

get_language <- function(file) {
  data <- jsonlite::fromJSON(file)

  language <- tibble::tibble(
    "name" = tolower(data$name),
    "date_order" = data$date_order,

    "skip" = list(
      tolower(unlist(data$skip)) %>%
        purrr::map_chr(supply_variants)
    ),

    "simplifications" = list(
      unlist(data$simplifications)
    )
  )

  n <- unlist(purrr::map(data, length))

  language <- dplyr::mutate(
    language, replacements = list(
      tibble::tibble(
        "before" = tolower(unlist(data)) %>%
          purrr::map_chr(supply_variants),
        "after" = rep(names(data), n)
      ) %>%
      dplyr::filter(
        !(after %in% colnames(language))
      )
    )
  )

  return(language)
}

supply_variants <- function(x) {
  variants <- stringr::str_sub(x, 1, 1) %>%
    sprintf("[%s|%s]", ., toupper(.))

  x <- paste0(variants, stringr::str_sub(x, 2)) %>%
    sprintf("(?<=[^\\p{L}]|^)%s(?=[^\\p{L}]|$)", .)

  return(x)
}

languages <- list.files("./data-raw", ".json$",
    full.names = TRUE, ignore.case = TRUE) %>%
  purrr::map(get_language) %>% dplyr::bind_rows()

usethis::use_data(languages, overwrite = TRUE)
