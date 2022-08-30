#' Set a Year and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for years.
#'
#' @examples
#' if (interactive()) {
#' x <- Year$new(1520)
#' x$take(15, type = "june")
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @importFrom lubridate interval ymd years days day
#' @importFrom lubridate int_standardize int_start
Year <- R6Class(
  classname = "Year",
  inherit = Periods,

  private = list(
    #' @description
    #' Helper function to specify a time period.
    .take_period = function(x, type) {
      if (!is.null(type)) {
        y <- rep(int_start(private$.interval), 2)

        y[1] <- y[1] + months(min(type) - 1)
        y[2] <- y[2] + months(max(type)) - days(1)

        if (x %in% 1:day(y[2])) {
          if (length(type) == 1) {
            lubridate::day(y[1]) <- as.integer(x)
            lubridate::day(y[2]) <- as.integer(x)
          }
        }

        return(interval(y[1], y[2]))
      }
    },

    #' @description
    #' Helper function to specify a season.
    .take_season = function(type) {
      x <- switch(
        type, spring = c(3, 4, 5), summer = c(6, 7, 8),
        autumn = c(9, 10, 11), winter = c(12, 13, 14)
      )

      return(x)
    },

    #' @description
    #' Helper function to specify a month.
    .take_month = function(type) {
      x <- pmatch(type, get_months(), nomatch = NULL)

      return(x)
    }
  ),

  public = list(
    #' @description
    #' Create a year.
    #'
    #' @param value A numerical scalar.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for years.
    initialize = function(value) {
      if (is.character(value)) value <- as.numeric(value)

      assertthat::assert_that(
        length(value) == 1 && floor(value) == value,
        value <= get_current_year()
      )

      if (value < 0) {
        value <- sprintf("%04d", abs(value))
        x <- ymd("0000-01-01") - years(value)
      } else {
        x <- ymd(sprintf("%04d-01-01", value))
      }

      x <- interval(x, x + years(1) - days(1))
      private$.interval <- int_standardize(x)
    },

    #' @description
    #' Specify a year.
    #'
    #' @param x A numerical scalar. The range of valid values
    #' depends on \code{type}. If \code{type} is \code{"spring"},
    #' \code{"summer"}, \code{"autumn"}, or \code{"winter"},
    #' \code{x} is ignored.
    #' @param type A character scalar. The following values are
    #' supported: \code{"spring"}, \code{"summer"}, \code{"autumn"},
    #' \code{"winter"}, and all English-language months.
    #' @param ignore_errors If `TRUE`, error messages are ignored.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for years.
    take = function(x = NA, type = NA, ignore_errors = FALSE) {
      suppressWarnings({
        tryCatch({
          if (length(x) == 2) type <- x[2]; x <- x[1]
          if (!is.na(x)) x <- as.numeric(x)

          assertthat::assert_that(length(type) == 1)
          type <- tolower(as.character(type))

          if (type %in% get_months()) {
            type <- private$.take_month(type)
          } else {
            type <- private$.take_season(type)
          }

          interval_x <- private$.take_period(x, type)
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
