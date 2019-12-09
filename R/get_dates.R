get_dates <- function(x, language) {
  x <- simplify(x, language = language)

  return(x)
}

#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter bind_rows
#' @importFrom purrr set_names
#' @importFrom rlang .data
simplify <- function(x, language) {
  language <- filter(get("languages"), .data$name %in% language)

  replacements <- bind_rows(language$simplifications)
  y <- set_names(replacements$after, replacements$pattern)

  return(str_replace_all(x, y))
}

build_century <- function(x, index = -1) {
  if (index > - 1) {
    x <- x[(index - 10):index]
    x <- x[!is.na(x)]
  }

  if ("and" %in% x) {
    and <- which(x == "and")[1]

    y <- get_century(x[1:(and - 1)])
    x <- get_century(x[(and + 1):length(x)])

    x <- Period$new(x, y)
  } else {
    x <- get_century(x)
  }

  return(x)
}

get_century <- function(x) {
  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Century$new(x[length(x)])$set_fuzzy(x)

  return(x_date$take(x_take))
}

build_decade <- function(x, index) {

}

get_decade <- function(x) {
  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Decade$new(x[length(x)])$set_fuzzy(x)

  return(x_date$take(x_take))
}
