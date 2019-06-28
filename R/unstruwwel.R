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
#' @importFrom assertthat not_empty
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
    if (!not_empty(language)) language <- guess_lang(x)
    assertthat::assert_that(is_valid_language(language))

    dates <- extract_numbers(x, remove = "\\.") %>%
      map(get_dates, language = language)
  } else {
    dates <- map(x, convert_midas)
  }

  return(dates)
}

#' @importFrom stringr str_remove_all str_match_all
#' @importFrom assertthat not_empty
extract_numbers <- function(x, remove = NULL) {
  if (is.list(remove)) remove <- unlist(remove)

  if (not_empty(remove)) {
    remove <- paste(remove, collapse = "|")
    x <- str_remove_all(x, pattern = remove)
  }

  x <- str_match_all(x, "([0-9]+)|([^\\s.]+)")

  return(x)
}
