#' Set a Decade and Get its Time Interval
#'
#' An Object of \code{R6Class} with methods to set
#' common time periods and specifications for decades.
#'
#' @examples
#' if (interactive()) {
#' x <- Decade$new(1520)
#' x$take(1, type = "half")
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @importFrom lubridate interval ymd years days
#' @importFrom lubridate int_standardize int_start int_end
Decade <- R6Class(
  classname = "Decade",
  inherit = Periods,

  private = list(
    #' @description
    #' Helper function to specify the beginning of a decade.
    .take_early = function() {
      if (int_start(private$.interval) < ymd("0000-01-01")) {
        y <- c(
          int_start(private$.interval) + years(8),
          int_end(private$.interval) - years(0)
        )
      } else {
        y <- c(
          int_start(private$.interval) + years(0),
          int_end(private$.interval) - years(8)
        )
      }

      return(interval(y[1], y[2]))
    },

    #' @description
    #' Helper function to specify the middle of a decade.
    .take_mid = function() {
      y <- c(
        int_start(private$.interval) + years(4),
        int_end(private$.interval) - years(4)
      )

      return(interval(y[1], y[2]))
    },

    #' @description
    #' Helper function to specify the end of a decade.
    .take_late = function() {
      if (int_start(private$.interval) < ymd("0000-01-01")) {
        y <- c(
          int_start(private$.interval) + years(0),
          int_end(private$.interval) - years(8)
        )
      } else {
        y <- c(
          int_start(private$.interval) + years(8),
          int_end(private$.interval) - years(0)
        )
      }

      return(interval(y[1], y[2]))
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
    #' @return Object of \code{R6Class} with methods to set
    #' common time periods and specifications for decades.
    initialize = function(value, official_def = FALSE) {
      if (is.character(value)) value <- as.numeric(value)

      assertthat::assert_that(
        length(value) == 1 && floor(value) == value,
        value <= get_current_year(), value %% 10 == 0
      )

      if (value < 0) {
        x <- ymd("0000-01-01") - years(abs(value))
        if (official_def) x <- x - years(1)
        x <- interval(x, x - years(8) - days(1))
      } else {
        x <- ymd(paste0(value, "-01-01"))
        if (official_def) x <- x + years(1)
        x <- interval(x, x + years(10) - days(1))
      }

      private$.interval <- int_standardize(x)
    }
  )
)
