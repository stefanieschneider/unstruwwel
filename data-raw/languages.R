require(magrittr)

get_language <- function(file) {
  data <- jsonlite::fromJSON(file)

  language <- tibble::tibble(
    "name" = data$name,
    "date_order" = data$date_order,

    "simplifications" = list(
      unlist(data$simplifications)
    )
  )

  n <- unlist(map(data, length))

  language <- dplyr::mutate(
    language, replacements = list(
      tibble::tibble(
        "before" = tolower(unlist(data)),
        "after" = rep(names(data), n)
      ) %>%
      dplyr::filter(
        !(after %in% colnames(language))
      )
    )
  )

  return(language)
}

languages <- list.files("./data-raw", ".json$",
    full.names = TRUE, ignore.case = TRUE) %>%
  map(get_language) %>% dplyr::bind_rows()

usethis::use_data(languages, overwrite = TRUE)
