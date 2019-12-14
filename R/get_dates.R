get_dates <- function(x, scheme, fuzzify) {
  markers <- sort(c(which(x == "century"), which(is_year(x))))

  if (length(markers) > 0) {
    if (length(markers) < length(x)) {
      test <- which(is_year_addition(x), useNames = FALSE)
      test <- (markers + 1) %in% c(test, "s")

      markers <- c(0, ifelse(test, markers + 1, markers))
      markers <- markers[markers < length(x)]

      if (markers[length(markers)] == 1) markers <- markers[1]

      x <- Map(
        function(i, j) get_intervals(x, start = i, end = j),
        markers + 1, cumsum(diff(c(markers, length(x))))
      )
    } else {
      x <- list(get_period(x, uncertain = FALSE))
    }

    if (scheme == "interval") x <- get_period(x)$interval
    if (scheme == "iso 8601") x <- get_period(x)$text
  } else {
    if (scheme == "interval") x <- c(NA, NA) else x <- NA
  }

  if (is.list(x)) return(x) else return(list(x))
}

get_intervals <- function(x, start = 1, end = -1) {
  test <- x[min(end + 1, length(x))] == "?"

  uncertain <- ifelse(test, TRUE, FALSE)
  number <- which(is_number(x[1:end]), FALSE)

  y <- x[start:number[length(number)]]; y <- y[y != "?"]
  next_char <- min(max(number) + 1, length(x))

  if ("century" %in% x[start:end]) {
    x <- build_century(y, uncertain = uncertain)
  } else if (x[next_char] == "s") {
    x <- get_decade(y, uncertain = uncertain)
  } else if (nchar(y[length(y)]) < 4) {
    x <- get_period(y, uncertain = uncertain)
  } else {
    x <- get_year(y, uncertain = uncertain)
  }

  return(x)
}

build_century <- function(x, uncertain = FALSE) {
  markers <- c(0, which(x == "and", useNames = FALSE) - 1)

  x <- Map(
    function(i, j) get_century(x[i:j]),
    markers + 1, cumsum(diff(c(markers, length(x))))
  )

  if (length(x) > 1) x <- Period$new(x)
  if (uncertain) x$fuzzy <- -1

  return(x)
}

get_century <- function(x, uncertain = FALSE) {
  if (uncertain) x <- c("?", x, use.names = FALSE)
  x <- x[max(1, length(x) - 5):length(x)]

  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Century$new(x[length(x)])$set_additions(x)

  return(x_date$take(x_take, ignore_errors = TRUE))
}

get_decade <- function(x, uncertain = FALSE) {
  if (uncertain) x <- c("?", x, use.names = FALSE)
  x <- x[max(1, length(x) - 5):length(x)]

  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Decade$new(x[length(x)])$set_additions(x)

  return(x_date$take(x_take, ignore_errors = TRUE))
}

get_period <- function(x, uncertain = FALSE) {
  if (uncertain) x <- c("?", x, use.names = FALSE)
  x <- x[max(1, length(x) - 5):length(x)]

  if (length(x) > 1) {
    y <- as.character(rev(x)[1:2]); n_char <- nchar(y)

    if (n_char[1] < n_char[2]) {
      add_char <- substr(y[2], 1, n_char[2] - n_char[1])
      x[length(x)] <- paste0(add_char, y[1])
    }
  }

  x_date <- Period$new(x)$set_additions(x)

  return(x_date)
}

get_year <- function(x, uncertain = FALSE) {
  if (uncertain) x <- c("?", x, use.names = FALSE)
  x <- x[max(1, length(x) - 5):length(x)]

  x_date <- Year$new(x[length(x)])$set_additions(x)

  return(x_date)
}
