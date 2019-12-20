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

      x[sapply(x, is.null)] <- NULL  # remove empty entries
    } else {
      x <- list(get_period(x, uncertain = FALSE))
    }

    if (scheme == "time-span") x <- get_period(x)$time_span
    if (scheme == "iso-format") x <- get_period(x)$iso_format
  } else {
    if (scheme == "time-span") x <- c(NA, NA) else x <- NA
  }

  if (is.list(x)) return(unlist(x)) else return(list(x))
}

get_intervals <- function(x, start = 1, end = -1) {
  test <- x[min(end + 1, length(x))] == "?"

  uncertain <- ifelse(test, TRUE, FALSE)
  number <- which(is_number(x[1:end]), FALSE)

  if (start <= number[length(number)]) {
    y <- x[start:number[length(number)]]; y <- y[y != "?"]
    next_char <- min(max(number) + 1, length(x))

    if ("century" %in% x[start:end]) {
      negative <- "bc" %in% x[max(end + 1, length(x))]
      x <- build_century(y, negative, uncertain)
    } else if (x[next_char] == "s") {
      x <- get_decade(y, uncertain = uncertain)
    } else if (nchar(y[length(y)]) < 4) {
      x <- get_period(y, uncertain = uncertain)
    } else {
      x <- get_year(y, uncertain = uncertain)
    }

    return(x)
  }
}

build_century <- function(x, negative, uncertain = FALSE) {
  markers <- c(0, which(x == "and", useNames = FALSE) - 1)

  x <- Map(
    function(i, j) get_century(x[i:j], negative),
    markers + 1, cumsum(diff(c(markers, length(x))))
  )

  if (length(x) > 1) x <- Periods$new(x)
  if (uncertain) x$fuzzy <- -1

  return(x)
}

get_century <- function(x, negative, uncertain = FALSE) {
  x <- x[max(1, length(x) - 4):length(x)]
  if (length(x) < 5) x <- c(rep(NA, 4), x)

  if (uncertain) x <- c("?", x, use.names = FALSE)
  if (negative) x[length(x)] <- paste0("-", x[length(x)])

  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Century$new(x[length(x)])$set_additions(x)

  return(x_date$take(x_take, ignore_errors = TRUE))
}

get_decade <- function(x, uncertain = FALSE) {
  x <- x[max(1, length(x) - 4):length(x)]
  if (length(x) < 5) x <- c(rep(NA, 4), x)

  if (uncertain) x <- c("?", x, use.names = FALSE)

  x_take <- c(x[length(x) - 2], x[length(x) - 1])
  x_date <- Decade$new(x[length(x)])$set_additions(x)

  return(x_date$take(x_take, ignore_errors = TRUE))
}

get_period <- function(x, uncertain = FALSE) {
  x <- x[max(1, length(x) - 4):length(x)]
  if (uncertain) x <- c("?", x, use.names = FALSE)

  if (length(x) > 1) {
    y <- as.character(rev(x)[1:2]); n_char <- nchar(y)

    if (n_char[1] < n_char[2]) {
      add_char <- substr(y[2], 1, n_char[2] - n_char[1])
      x[length(x)] <- paste0(add_char, y[1])
    }
  }

  x_date <- Periods$new(x)$set_additions(x)

  return(x_date)
}

get_year <- function(x, uncertain = FALSE) {
  x <- x[max(1, length(x) - 4):length(x)]
  if (length(x) < 5) x <- c(rep(NA, 4), x)

  if (uncertain) x <- c("?", x, use.names = FALSE)

  if (is_number(x[length(x) - 1])) {
    x_take <- c(x[length(x) - 1], x[length(x) - 2])
  } else {
    x_take <- c(x[length(x) - 2], x[length(x) - 1])
  }

  x_date <- Year$new(x[length(x)])$set_additions(x)

  return(x_date$take(x_take, ignore_errors = TRUE))
}
