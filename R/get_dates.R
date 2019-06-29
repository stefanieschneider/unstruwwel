get_dates <- function(x, language) {
  colnames(x) <- c("value", "digits", "letters")

  x <- tibble::as_tibble(x, .name_repair = "unique") %>%
    dplyr::mutate(letters = tolower(letters))

  return(x)
}

simplify <- function(x) {

}

is_suffix <- function(x) {

}

is_year <- function(x) {

}

is_season <- function(x) {

}

is_month <- function(x) {

}

is_day <- function(x) {

}

is_prefix <- function(x) {

}
