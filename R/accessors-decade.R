#' Set a Decade and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for decades.
#'
#' @examples
#' \donttest{
#'   x <- Decade$new(1520)
#'   x$take(1, type = "half")
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
    #' @description
    #' Helper function to specify the beginning of a decade.
    .take_early = function() {
      interval <- c(
        private$.interval[1],
        private$.interval[1] + 1
      )

      return(interval)
    },

    #' @description
    #' Helper function to specify the middle of a decade.
    .take_mid = function() {
      interval <- c(
        private$.interval[1] + 4,
        private$.interval[2] - 4
      )

      return(interval)
    },

    #' @description
    #' Helper function to specify the end of a decade.
    .take_late = function() {
      interval <- c(
        private$.interval[2] - 1,
        private$.interval[2]
      )

      return(interval)
    }
  ),

  public = list(
    #' @description
    #' Create a decade.
    #'
    #' @param value A numerical scalar.
    #' @param official_def If `TRUE`, the official definition that
    #' begins with the year 1 is used.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for decades.
    initialize = function(value, official_def = FALSE) {
      if (is.character(value)) value <- as.numeric(value)

      assertthat::assert_that(
        length(value) == 1 && floor(value) == value,
        value <= get_current_year()
      )

      if (value < 0) private$.negative <- TRUE

      if (value %% 10 != 0) {
        assertthat::assert_that(value < 202)
        value <- abs(value) * 10 + 1
      }

      private$.interval <- c(abs(value), abs(value) + 9)

      if (official_def && value %% 10 == 0)
        private$.interval <- private$.interval + 1
    }
  )
)
