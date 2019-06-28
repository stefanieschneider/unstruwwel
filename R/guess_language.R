#' @importFrom stringr str_extract_all
#' @importFrom dplyr rename filter
#' @importFrom purrr map set_names
#' @importFrom magrittr "%>%"
guess_lang <- function(x) {
  languages <- get("languages")

  x <- str_extract_all(x, "[\\p{L}]+") %>%
    unlist() %>% .[nchar(.) > 1]

  language <- map(languages$replacements,
      ~ sum(x %in% .$before) / length(x)) %>%
    set_names(languages$name) %>% unlist() %>%
    .[. > 0.5] %>% sort(decreasing = TRUE)

  if (length(language) == 0) {
    text <- paste(
      "Language could not be detected.",
      "Do you want to proceed with English?"
    )

    input <- menu(c("Yes", "No"), title = text)
    if (input == 1) language <- "en"
  }

  if (length(language) > 1) {
    text <- paste(
      "Multiple languages have been detected.",
      "With which language do you want to continue?"
    )

    input <- menu(names(language), title = text)
    language <- language[input]
  }

  assertthat::assert_that(length(language) == 1)

  return(names(language))
}
