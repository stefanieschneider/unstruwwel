#' @importFrom stringr str_extract_all str_subset
#' @importFrom magrittr "%>%"
#' @import dplyr
guess_language <- function(x, verbose = TRUE) {
  language <- NULL # initializer
  languages <- get("languages")

  # combinations of numbers and letters
  valid_chars <- "[\\p{L}]{2,15}"

  x <- str_subset(x, "[0-9]") %>%
    str_subset(valid_chars)

  if (length(x) > 0) {
    # fill sparse replacements for English
    languages <- mutate(
      languages, replacements = case_when(
        name == "en" ~ list(
          bind_rows(languages$replacements) %>%
          pull(after) %>% unique() %>%
          tibble::enframe(
            name = NULL, value = "before"
          ) %>%
          bind_rows(
            filter(languages, name == "en") %>%
              pull(replacements) %>% pluck(1)
          ) %>%
          group_by(before) %>% sample_n(1)
        ),
        !is.na(name) ~ replacements
      )
    )

    # sample and extract selected combinations
    text <- str_extract_all(sample_text(x), valid_chars)

    # aggregate and filter dominant languages
    language <- purrr::map(languages$replacements,
        ~ count_language(.$before, text = text)) %>%
      purrr::set_names(languages$name) %>% unlist()

    language <- language[language > max(language) / 2]
  }

  if (length(language) == 0) {
    if (interactive()) {
      text <- paste(
        "Language could not be detected with certainty.",
        "Do you want to proceed with English?"
      )

      input <- utils::menu(c("Yes", "No"), title = text)
      if (input == 1) language <- "en"
    }
  }

  assertthat::assert_that(length(language) > 0)

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

#' @importFrom magrittr "%>%"
count_language <- function(language, text) {
  text <- purrr::map(text, ~ tolower(.) %>%
    intersect(language) %>% length())

  return(sum(unlist(text)))
}
