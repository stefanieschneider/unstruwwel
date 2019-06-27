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
#' \donttest{}
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

  # dates as defined in midas are language-independent
  if (check_midas(x, midas = midas)) language <- "en"
  if (!not_empty(language)) language <- guess_lang(x)

  assertthat::assert_that(
    not_empty(language), is_language(language)
  )

  if (!midas) {
    dates <- extract_numbers(x) %>%
      map(get_dates, language = language)
  }

  return(dates)
}

#' @importFrom magrittr "%>%"
extract_numbers <- function(x) {
  x <- stringr::str_remove_all(x, "\\.") %>%
    stringr::str_match_all("([0-9]+)|([^\\s.]+)")

  return(x)
}
