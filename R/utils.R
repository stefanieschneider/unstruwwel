#' @importFrom utf8 utf8_normalize
normalize <- function(x) {
  result <- purrr::set_names(x, utf8_normalize(colnames(x))) %>%
    dplyr::mutate_if(is.character, list(~ utf8_normalize(.)))

  return(result)
}

is_valid_language <- function(x) {
  return(all(x %in% get("languages")$name))
}

get_invalid_language <- function(x) {
  return(x[!(x %in% get("languages")$name)][1])
}

is_year <- function(x) {
  return(x %in% 1000:2100)
}

is_season <- function(x) {
  seasons <- c(
    "winter", "spring", "summer", "autumn"
  )

  return(x %in% seasons)
}

is_month <- function(x) {
  months <- c(
    "january", "february", "march", "april", "may",
    "june", "july", "august", "september", "october",
    "november", "december"
  )

  return(x %in% months)
}

is_day <- function(x) {
  days <- c(
    "monday", "tuesday", "wednesday", "thursday",
    "friday", "saturday", "sunday"
  )

  return(x %in% days)
}

is_suffix <- function(x) {

}

is_prefix <- function(x) {

}

is_uncertain <- function(x) {
  return(x %in% c("uncertain", "?"))
}

is_approximate <- function(x) {
  return(x %in% c("approximate"))
}

is_or <- function(x) {
  return(x %in% c("or"))
}

is_and <- function(x) {
  return(x %in% c("and", "/"))
}
