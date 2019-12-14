#' @importFrom dplyr mutate filter case_when group_by summarise
#' @importFrom stringr str_extract_all str_subset
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
guess_language <- function(x, verbose = TRUE) {
  valid_chars <- "[\\p{L}]{3,}" # three letters minimum
  language <- NULL # initialize empty default

  languages <- get("languages")

  # filter combinations of numbers and letters
  x <- str_subset(str_subset(x, "[0-9]"), valid_chars)

  if (length(x) > 0) {
    # sample and extract selected combinations
    text <- str_extract_all(sample_text(x), valid_chars)

    # aggregate and filter dominant languages
    language <- count_per_language(languages, text) %>%
      tidyr::gather("key", "value", -("id")) %>%
      mutate(max_value = max(.data$value)) %>%
      filter(
        sum(.data$value == .data$max_value) == 1
      ) %>%
      mutate(
        value = case_when(
          .data$value == .data$max_value ~ 1,
          !is.na(.data$value) ~ 0
        )
      ) %>%
      group_by(.data$key) %>%
      summarise(value = sum(.data$value)) %>%
      filter(.data$value > max(.data$value) / 2)
  }

  assertthat::assert_that(
    nrow(language) > 0, msg = "Language could not be detected."
  )

  language <- purrr::set_names(language$value, language$key)

  if (verbose) {
    message(
      sprintf(
        "The following languages have been detected: %s.",
        paste(names(language), collapse = ", ")
      )
    )
  }

  return(names(language))
}

sample_text <- function(x, n = 100) {
  replace <- ifelse(length(x) < n, TRUE, FALSE)

  return(sample(x, n, replace = replace))
}

#' @importFrom dplyr bind_rows group_by
#' @importFrom tibble rowid_to_column
#' @importFrom purrr map set_names
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
count_per_language <- function(x, text) {
  x <- map(
      x$replacements, ~ count_language(
        .$before, text = text
      )
    ) %>%
    set_names(x$name) %>% bind_rows() %>%
    rowid_to_column("id") %>% group_by(.data$id)

  return(x)
}

#' @importFrom magrittr "%>%"
count_language <- function(language, text) {
  text <- purrr::map(text, ~ tolower(.) %>%
    intersect(language) %>% length())

  return(unlist(text))
}
