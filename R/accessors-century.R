#' Set a Century and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for centuries.
#'
#' @examples
#' \donttest{
#'   x <- Century$new(15)
#'   x$take(2, type = "third")
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
    #' @description
    #' Helper function to specify the beginning of a century.
    .take_early = function() {
      interval <- c(
        private$.interval[1],
        private$.interval[1] + 14
      )

      return(interval)
    },

    #' @description
    #' Helper function to specify the middle of a century.
    .take_mid = function() {
      interval <- c(
        private$.interval[1] + 45,
        private$.interval[2] - 45
      )

      return(interval)
    },

    #' @description
    #' Helper function to specify the end of a century.
    .take_late = function() {
      interval <- c(
        private$.interval[2] - 14,
        private$.interval[2]
      )

      return(interval)
    }
  ),

  public = list(
    #' @description
    #' Create a century.
    #'
    #' @param value A numerical scalar.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for centuries.
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
