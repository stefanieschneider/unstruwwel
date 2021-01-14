#' Set a Period and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for time periods.
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @importFrom lubridate as_date interval ymd years year days
#' @importFrom lubridate int_standardize int_start int_end
Periods <- R6Class(
  classname = "Periods",

  private = list(
    #' @field .interval Stores a time interval.
    .interval = NA_character_,

    #' @description
    #' Helper function to specify a time period.
    #'
    #' @param x A numerical scalar. The range of valid values
    #' depends on \code{type}. If \code{type} is \code{"early"},
    #' \code{"mid"}, or \code{"late"}, \code{x} is ignored.
    #' @param type A character scalar. The following values
    #' are supported: \code{"early"}, \code{"mid"}, \code{"late"},
    #' \code{"quarter"}, \code{"third"}, and \code{"half"}. If
    #' \code{type} is `NULL`, \code{x} defines a year or decade.
    .take_period = function(x, type) {
      max_value <- switch(type, quarter = 4, third = 3, half = 2, 10)
      if (x == "last") x <- max_value; if (x == "first") x <- 1

      assertthat::assert_that(length(x) == 1, x %in% 1:max_value)

      if (int_start(private$.interval) < ymd("0000-01-01")) {
        n_years <- private$.interval / years(1) + 2
        step <- round(n_years / max_value, 0)

        y <- c(
          int_end(private$.interval) - years((x - 1) * step),
          int_end(private$.interval) - years(x * step - 2) - days(1)
        )

        if (x == max_value && step %% 3 == 0) y[2] <- y[2] - years(1)
      } else {
        n_years <- private$.interval / years(1)
        step <- round(n_years / max_value, 0)

        y <- c(
          int_start(private$.interval) + years((x - 1) * step),
          int_start(private$.interval) + years(x * step) - days(1)
        )

        if (x == max_value && step %% 3 == 0) y[2] <- y[2] + years(1)
      }

      return(interval(y[1], y[2]))
    }
  ),

  active = list(
    #' @field interval Convert and return a POSIXt time interval.
    interval = function(value) {
      if (missing(value)) {
        interval_x <- int_standardize(private$.interval)

        if (self$express != 0) {
          if (self$express < 0) {
            start_x <- ymd("0000-01-01") - years(9999)
            end_x <- int_start(interval_x) - days(1)
          }

          if (self$express > 0) {
            start_x <- int_end(interval_x) + days(1)
            end_x <- ymd("0000-01-01") + years(9999)
          }

          interval_x <- interval(start_x, end_x)
        }

        return(interval_x)
      } else {
        stop("`$interval` is read only.", FALSE)
      }
    },

    #' @field time_span Convert and return a time span in years.
    time_span = function(value) {
      if (missing(value)) {
        x <- c(
          year(int_start(self$interval)),
          year(int_end(self$interval))
        )

        if (x[1] == -9999) x[1] <- -Inf
        if (x[2] == 9999) x[2] <- Inf

        return(sort(x, decreasing = FALSE))
      } else {
        stop("`$time_span` is read only.", FALSE)
      }
    },

    #' @field iso_format Convert and return a date in ISO 8601.
    iso_format = function(value) {
      if (missing(value)) {
        x <- c(
          stringr::str_pad(int_start(self$interval), 10, pad = "0"),
          stringr::str_pad(int_end(self$interval), 10, pad = "0")
        )

        if (self$fuzzy < 0) x <- paste0(x, "~", collapse = NULL)
        if (self$fuzzy > 0) x <- paste0(x, "?", collapse = NULL)

        if (year(int_start(self$interval)) == -9999)
          x <- paste("..", x[2], sep = "", collapse = "")

        if (year(int_end(self$interval)) == 9999)
          x <- paste(x[1], "..", sep = "", collapse = "")

        return(paste(x, collapse = "/"))
      } else {
        stop("`$text` is read only.", FALSE)
      }
    }
  ),

  public = list(
    #' @field fuzzy Either `-1` (approximate) or `1` (uncertain).
    fuzzy = 0,

    #' @field express Either `-1` (before) or `1` (after).
    express = 0,

    #' @description
    #' Create a time period.
    #'
    #' @param ... Intervals, numerical scalars, or objects of
    #' class \code{Period}.
    initialize = function(...) {
      if ("Interval" %in% class(...)) {
        private$.interval <- Reduce(union, list(...))
      } else {
        x <- unlist(list(...), recursive = TRUE)
        x <- x[!is.na(x) & length(x) > 0]

        fuzzy <- unlist(lapply(x, function(x) {
          if ("Periods" %in% class(x)) x$fuzzy}))

        x <- lapply(x, function(x) {
          if ("Periods" %in% class(x)) x$interval
          else Year$new(x)$interval})

        assertthat::assert_that(length(x) > 0)

        private$.interval <- interval(
          Reduce(min, lapply(x, int_start)),
          Reduce(max, lapply(x, int_end))
        )

        if (length(fuzzy) > 0) {
          if (any(fuzzy < 0)) self$fuzzy <- -1
          if (any(fuzzy > 0)) self$fuzzy <- 1
        }
      }
    },

    #' @description
    #' Set additions for a time period.
    #'
    #' @param x A character vector.
    set_additions = function(x) {
      if (any(c("approximate", "?") %in% x)) self$fuzzy <- -1
      if (any(c("uncertain") %in% x)) self$fuzzy <- 1

      if ("before" %in% x) self$express <- -1
      if ("after" %in% x) self$express <- 1

      return(self)
    },

    #' @description
    #' Specify a period.
    #'
    #' @param x A numerical scalar. The range of valid values
    #' depends on \code{type}. If \code{type} is \code{"early"},
    #' \code{"mid"}, or \code{"late"}, \code{x} is ignored.
    #' @param type A character scalar. The following values
    #' are supported: \code{"early"}, \code{"mid"}, \code{"late"},
    #' \code{"quarter"}, \code{"third"}, and \code{"half"}. If
    #' \code{type} is `NULL`, \code{x} defines a year or decade.
    #' @param ignore_errors If `TRUE`, error messages are ignored.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for time periods.
    take = function(x = NA, type = NA, ignore_errors = FALSE) {
      suppressWarnings({
        tryCatch({
          if (length(x) == 2) type <- x[2]; x <- x[1]
          if (!is.na(x) & x != "last") x <- as.numeric(x)

          assertthat::assert_that(length(type) == 1)
          type <- tolower(as.character(type))

          interval_x <- switch(type,
            early = private$.take_early(),
            late = private$.take_late(),
            mid = private$.take_mid(),

            private$.take_period(x, type)
          )

          period_x <- Periods$new(interval_x)

          period_x$fuzzy <- self$fuzzy
          period_x$express <- self$express

          return(period_x)
        }, error = function(event) {
          if (ignore_errors) return(self)
          else stop(event)
        })
      })
    }
  )
)
