#' Detect and Parse Historic Dates
#'
#' Detect and Parse Historic Dates, e.g., to ISO 8601:2-2019.
#'
#' @param x Input vector. Either a character vector, or something
#' coercible to one.
#' @param language Language code of the input vector as defined in
#' ISO 639-1. If \code{NULL}, language is detected automatically.
#' @param midas If \code{TRUE}, input was standardized using MIDAS
#' (Marburger Informations-, Dokumentations- und Administrations-
#' System). See \url{https://doi.org/10.11588/artdok.00003770}.
#' @param verbose If \code{TRUE}, additional diagnostics are printed.
#' @param scheme Scheme code of the output list. Either \code{time-span},
#' \code{iso-format}, or \code{object}.
#' @param fuzzify A numerical vector of length 2 to extend the interval
#' of approximate or uncertain time periods. This is only applied if
#' \code{scheme == "time-span"}.
#'
#' @return A named list of vectors or objects of \code{\link{R6Class}}.
#'
#' @examples
#' if (interactive()) {
#' unstruwwel("1. Hälfte 19. Jahrhundert", language = "de")
#' unstruwwel("circa between 1901 and 1905", language = "en")
#' }
#'
#' @note Although multiple languages can be detected, only dominant
#' ones are ultimately set.
#'
#' @importFrom purrr map set_names
#' @importFrom magrittr "%>%"
#'
#' @rdname unstruwwel
#' @export
unstruwwel <- function(x, language = NULL, midas = FALSE, verbose = TRUE,
    scheme = "time-span", fuzzify = c(0, 0)
) {
  x <- unlist(x); scheme <- tolower(scheme)
  language <- unlist(language, recursive = TRUE)

  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is.logical(midas))
  assertthat::assert_that(is.logical(verbose))

  assertthat::assert_that(is.numeric(fuzzify), length(fuzzify) == 2)
  assertthat::assert_that(scheme %in% c("iso-format", "time-span", "object"))

  if (!guess_midas(x, midas = midas, verbose = verbose)) {
    if (is.null(language)) language <- guess_language(x, verbose)
    assertthat::assert_that(is.vector(language), is.character(language))

    assertthat::assert_that(
      is_valid_language(language), msg = sprintf(
        paste("`%s` is either not defined in ISO 639-1 or not yet",
          "implemented."), get_invalid_language(language)
      )
    )

    dates <- standardize_vector(x, language, "\\.(?=[^0-9]|$)") %>%
      extract_groups() %>% map(get_dates, scheme, fuzzify)
  } else {
    dates <- map(x, convert_midas) # language-independent
  }

  names(dates) <- x

  return(dates)
}

#' @importFrom stringr str_remove_all str_replace_all str_squish
#' @importFrom dplyr filter bind_rows distinct
#' @importFrom purrr set_names
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
standardize_vector <- function(x, language, remove = NULL) {
  language <- filter(get("languages"), .data$name %in% language)
  remove <- unlist(append(remove, language$stop_words), TRUE)

  if (assertthat::not_empty(remove))
    x <- str_remove_all(x, paste(remove, collapse = "|"))

  replacements <- bind_rows(language$replacements) %>%
    filter(.data$before != .data$after) %>% distinct()

  # simplification <- bind_rows(language$simplifications)

  x <- utf8::utf8_normalize(x) %>% str_squish() %>%
    str_replace_all(
      set_names(replacements$after, replacements$pattern)
    )

  return(x)
}

#' @importFrom stringr str_extract_all
extract_groups <- function(x) {
  capture_groups <- "([0-9]+)|(\\p{L}+)|(\\?)"
  x <- str_extract_all(x, capture_groups)

  return(x)
}
