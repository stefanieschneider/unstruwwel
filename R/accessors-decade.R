#' Set a Decade and Get its Time Interval
#'
#' @return Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for decades.
#' @format An \code{\link{R6Class}} object.
#'
#' @examples
#' \donttest{
#'   x <- Decade$new(1520)
#'   x$take(1, type = "half")
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
#'     \code{type} is `NULL`, \code{x} defines a year.}
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
Decade <- R6Class(
  classname = "Decade",
  inherit = Period,

  private = list(
    .take_early = function() {
      interval <- c(
        private$.interval[1],
        private$.interval[1] + 1
      )

      return(interval)
    },

    .take_mid = function() {
      interval <- c(
        private$.interval[1] + 4,
        private$.interval[2] - 4
      )

      return(interval)
    },

    .take_late = function() {
      interval <- c(
        private$.interval[2] - 1,
        private$.interval[2]
      )

      return(interval)
    }
  ),

  public = list(
    initialize = function(value) {
      assertthat::assert_that(
        length(value) == 1 && floor(value) == value,
        value <= get_current_year()
      )

      if (value < 0) private$.negative <- TRUE

      if (value %% 10 != 0) {
        assertthat::assert_that(value < 202)
        value <- abs(value) * 10 + 1
      }

      private$.interval <- c(
        abs(value), abs(value) + 9
      )
    }
  )
)
