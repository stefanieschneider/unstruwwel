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
        "%s is either not defined in ISO 639-1 or not yet
        implemented.", get_invalid_language(language)
      )
    )

    dates <- standardize_vector(x, language, "\\.(?=[^0-9]|$)") %>%
      extract_numbers() %>% map(get_dates, language = language)
  } else {
    dates <- map(x, convert_midas) # language-independent
  }

  return(dates)
}

#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @import stringr dplyr
standardize_vector <- function(x, language, remove = NULL) {
  language <- filter(get("languages"), .data$name %in% language)
  replacements <- bind_rows(language$replacements) %>% distinct()

  remove <- unlist(append(remove, language$stop_words))

  if (assertthat::not_empty(remove)) {
    x <- str_remove_all(x, paste(remove, collapse = "|"))
  }

  x <- str_replace_all(
    str_squish(x), purrr::set_names(
      replacements$after, replacements$pattern
    )
  )

  return(x)
}

extract_numbers <- function(x) {
  return(stringr::str_match_all(x, "([0-9]+)|([^\\s.]+)"))
}
