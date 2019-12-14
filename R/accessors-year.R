#' Set a Year and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for years.
#'
#' @examples
#' \donttest{
#'   x <- Year$new(1520)
#'   x$take(type = "autumn")
#' }
#'
#' @keywords date time
#' @docType class
#'
#' @importFrom R6 R6Class
Year <- R6Class(
  classname = "Year",
  inherit = Period,

  private = list(
    #' @field .season Stores a season.
    .season = NA_integer_,

    #' @field .month Stores a month.
    .month = NA_integer_,

    #' @field .day Stores a day.
    .day = NA_integer_
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

      if (value < 0) private$.negative <- TRUE
      private$.interval <- c(abs(value), abs(value))
    }
  )
)
