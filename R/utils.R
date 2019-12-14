is_valid_language <- function(x) {
  return(all(x %in% get("languages")$name))
}

get_invalid_language <- function(x) {
  return(x[!(x %in% get("languages")$name)][1])
}

get_current_year <- function() {
  return(as.integer(format(Sys.Date(), "%Y")))
}

is_year <- function(x) {
  test <- as.character(1000:get_current_year())

  return(as.character(x) %in% test)
}

is_year_addition <- function(x) {
  return(as.character(x) %in% as.character(1:999))
}

is_number <- function(x) {
  return(suppressWarnings(!is.na(as.numeric(x))))
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

get_item <- function(x, n = 1) {
  return(x[[1]][[n]])
}
