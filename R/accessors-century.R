#' Set a Century and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for centuries.
#'
#' @examples
#' \donttest{
#' x <- Century$new(15)
#' x$take(2, type = "third")
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @importFrom lubridate interval ymd years days
#' @importFrom lubridate int_standardize int_start int_end
Century <- R6Class(
  classname = "Century",
  inherit = Periods,

  private = list(
    #' @description
    #' Helper function to specify the beginning of a century.
    .take_early = function() {
      if (int_start(private$.interval) < ymd("0000-01-01")) {
        y <- c(
          int_start(private$.interval) + years(85),
          int_end(private$.interval) - years(0)
        )
      } else {
        y <- c(
          int_start(private$.interval) + years(0),
          int_end(private$.interval) - years(85)
        )
      }

      return(interval(y[1], y[2]))
    },

    #' @description
    #' Helper function to specify the middle of a century.
    .take_mid = function() {
      y <- c(
        int_start(private$.interval) + years(45),
        int_end(private$.interval) - years(45)
      )

      return(interval(y[1], y[2]))
    },

    #' @description
    #' Helper function to specify the end of a century.
    .take_late = function() {
      if (int_start(private$.interval) < ymd("0000-01-01")) {
        y <- c(
          int_start(private$.interval) + years(0),
          int_end(private$.interval) - years(85)
        )
      } else {
        y <- c(
          int_start(private$.interval) + years(85),
          int_end(private$.interval) - years(0)
        )
      }

      return(interval(y[1], y[2]))
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
        value < 22 && nchar(abs(value)) < 3
      )

      if (value < 0) {
        value <- sprintf("%02d01", abs(value + 1))

        x <- ymd("0000-01-01") - years(value)
        x <- interval(x, x - years(98) - days(1))
      } else {
        x <- ymd(sprintf("%02d01-01-01", value - 1))
        x <- interval(x, x + years(100) - days(1))
      }

      private$.interval <- int_standardize(x)
    }
  )
)
