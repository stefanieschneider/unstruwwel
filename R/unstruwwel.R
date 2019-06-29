#' Detect and Parse Historic Dates
#'
#' Detect and Parse Historic Dates, e.g., to ISO 8601:2-2019.
#'
#' @param x Input vector. Either a character vector, or something
#' coercible to one.
#' @param language Language code of the input vector as defined in
#' ISO 639-1. Defaults to `en`, i.e, English.
#' @param midas If `TRUE`, input vector was standardized using MIDAS
#' (Marburger Informations-, Dokumentations- und Administrations-
#' System). See \url{https://doi.org/10.11588/artdok.00003770}.
#' @param ... Optional arguments (currently not used).
#' @return A named list of vectors.
#'
#' @examples
#' unstruwwel("1. Hälfte 19. Jahrhundert", language = "de")
#'
#' @importFrom purrr map set_names
#' @importFrom magrittr "%>%"
#'
#' @rdname unstruwwel
#' @export
unstruwwel <- function(x, language = "en", midas = FALSE, ...) {
  if (is.list(x)) x <- unlist(x)

  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.logical(midas))

  if (!guess_midas(x, midas = midas)) {
    if (!assertthat::not_empty(language)) language <- guess_lang(x)
    assertthat::assert_that(is_valid_language(language))

    dates <- standardize_vector(x, language, "\\.(?=\\s|$)") %>%
      extract_numbers() %>% map(get_dates, language = language)
  } else {
    dates <- map(x, convert_midas) # language-independent
  }

  return(dates)
}

#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom purrr pluck set_names
#' @importFrom magrittr "%>%"
standardize_vector <- function(x, language, remove = NULL) {
  language <- dplyr::filter(get("languages"), name == language)
  remove <- unlist(append(remove, pluck(language$skip, 1)))

  if (assertthat::not_empty(remove)) {
    x <- str_remove_all(x, paste(remove, collapse = "|"))
  }

  x <- str_replace_all(
    str_squish(x), set_names(
      pluck(language$replacements, 1, 2), # replacements
      pluck(language$replacements, 1, 1)  # patterns
    )
  )

  return(x)
}

extract_numbers <- function(x) {
  return(stringr::str_match_all(x, "([0-9]+)|([^\\s.]+)"))
}
