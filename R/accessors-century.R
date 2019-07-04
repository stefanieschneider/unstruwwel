#' Set a Century and Get its Time Interval
#'
#' @return Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for centuries.
#' @format An \code{\link{R6Class}} object.
#'
#' @examples
#' \donttest{
#'   x <- Century$new(15)
#'   x$take(2, type = "third")
#' }
#'
#' @field interval Stores a time interval.
#'
#' @section Arguments:
#' \describe{
#'   \item{\code{x}}{A numerical scalar. The range of valid values
#'     depends on \code{type}. If \code{type} is \code{"early"},
#'     \code{"mid"}, or \code{"late"}, \code{x} is ignored.}
#'   \item{\code{type}}{A character scalar. The following values
#'     are supported: \code{"early"}, \code{"mid"}, \code{"late"},
#'     \code{"quarter"}, \code{"third"}, and \code{"half"}. If
#'     \code{type} is `NULL`, \code{x} defines a decade.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{take()}}{Set a time period or specification.}
#' }
#'
#' @keywords date time
#' @docType class
#'
#' @importFrom R6 R6Class
Century <- R6Class("Century",
  private = list(
    .interval = NA_integer_,
    .negative = FALSE,

    .take_period = function(x, type) {
      assertthat::assert_that(
        type %in% c("quarter", "third", "half")
      )

      max_value <- switch(type,
        quarter = 4, third = 3, half = 2
      )

      if (x == "last") x <- max_value

      assertthat::assert_that(x %in% 1:max_value)

      step <- floor(100 / max_value)

      interval <- c(
        (x - 1) * step + private$.interval[1],
        x * step + private$.interval[1] - 1
      )

      if (x == 3 && step %% 3 == 0) {
        interval[2] <- interval[2] + 1
      }

      return(interval)
    },

    .take_early = function() {
      interval <- c(
        private$.interval[1],
        private$.interval[1] + 14
      )

      return(interval)
    },

    .take_mid = function() {
      interval <- c(
        private$.interval[1] + 45,
        private$.interval[2] - 45
      )

      return(interval)
    },

    .take_late = function() {
      interval <- c(
        private$.interval[2] - 14,
        private$.interval[2]
      )

      return(interval)
    },

    .take_decade = function(x) {
      if (x > 9 && x %% 10 == 0) x <- x / 10

      assertthat::assert_that(x %in% 0:9)

      interval <- c(
        x * 10 + private$.interval[1] - 1,
        x * 10 + private$.interval[1] + 8
      )

      return(interval)
    }
  ),

  active = list(
    interval = function(value) {
      if (missing(value)) {
        negative <- ifelse(private$.negative, -1, 1)

        return(sort(private$.interval * negative))
      } else {
        stop("`$interval` is read only.", FALSE)
      }
    }
  ),

  public = list(
    initialize = function(value) {
      assertthat::assert_that(
        length(value) == 1 && floor(value) == value,
        value < 22 || is_year(value)
      )

      if (value < 0) private$.negative <- TRUE

      if (is_year(value)) {
        value <- stringr::str_sub(value, end = 2)
        value <- as.integer(value) + 1
      }

      private$.interval <- c(
        abs(value) * 100 - 99, abs(value) * 100
      )
    },

    take = function(x = NULL, type = NULL) {
      if (!is.null(type)) {
        assertthat::assert_that(length(type) == 1)

        type <- tolower(as.character(type))

        interval <- switch(type,
          early = private$.take_early(),
          late = private$.take_late(),
          mid = private$.take_mid(),

          private$.take_period(x, type)
        )
      } else {
        interval <- private$.take_decade(x)
      }

      negative <- ifelse(private$.negative, -1, 1)

      return(sort(interval * negative))
    }
  )
)
