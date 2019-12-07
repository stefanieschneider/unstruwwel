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
Century <- R6Class(
  classname = "Century",
  inherit = Period,

  private = list(
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
    }
  ),

  public = list(
    initialize = function(value) {
      if (is.character(value)) value <- as.numeric(value)

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
    }
  )
)
