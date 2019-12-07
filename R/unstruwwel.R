#' Detect and Parse Historic Dates
#'
#' Detect and Parse Historic Dates, e.g., to ISO 8601:2-2019.
#'
#' @param x Input vector. Either a character vector, or something
#' coercible to one.
#' @param midas If `TRUE`, input vector was standardized using MIDAS
#' (Marburger Informations-, Dokumentations- und Administrations-
#' System). See \url{https://doi.org/10.11588/artdok.00003770}.
#' @param language Language code of the input vector as defined in
#' ISO 639-1. If `NULL`, language is detected automatically.
#' @param verbose If `TRUE`, additional diagnostics are printed.
#' @return A named list of vectors.
#'
#' @examples
#' unstruwwel("1. Hälfte 19. Jahrhundert", language = "de")
#'
#' @note Although multiple languages can be detected, only dominant
#' ones are ultimately set.
#'
#' @importFrom purrr map set_names
#' @importFrom magrittr "%>%"
#'
#' @rdname unstruwwel
#' @export
unstruwwel <- function(x, midas = FALSE, language = NULL, verbose = TRUE) {
  x <- unlist(x); language <- unlist(language)

  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.logical(midas))
  assertthat::assert_that(is.vector(language))
  assertthat::assert_that(is.logical(verbose))

  if (!guess_midas(x, midas = midas, verbose = verbose)) {
    if (is.null(language)) language <- guess_language(x, verbose)

    assertthat::assert_that(
      is_valid_language(language), msg = sprintf(
        paste("`%s` is either not defined in ISO 639-1 or not yet",
          "implemented."), get_invalid_language(language)
      )
    )

    dates <- standardize_vector(x, language, "\\.(?=[^0-9]|$)") %>%
      extract_groups() %>% map(get_dates, language = language)
  } else {
    dates <- map(x, convert_midas) # language-independent
  }

  return(dates)
}

#' @importFrom stringr str_remove_all str_replace_all str_squish
#' @importFrom dplyr filter bind_rows distinct
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
standardize_vector <- function(x, language, remove = NULL) {
  language <- filter(get("languages"), .data$name %in% language)
  remove <- unlist(append(remove, language$stop_words), TRUE)

  if (assertthat::not_empty(remove)) {
    x <- str_remove_all(x, paste(remove, collapse = "|"))
  }

  replacements <- bind_rows(language$replacements) %>%
    filter(.data$before != .data$after) %>% distinct()

  x <- utf8::utf8_normalize(x) %>% str_squish() %>%
    str_replace_all(
      purrr::set_names(
        replacements$after, replacements$pattern
      )
    )

  return(x)
}

#' @importFrom stringr str_match_all
extract_groups <- function(x) {
  # numerals, letters, and special characters
  capture_groups <- "([0-9]+)|(\\p{L}+)|([^\\s])"

  return(str_match_all(x, capture_groups))
}
