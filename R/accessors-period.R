#' Set a Period
#'
#' @return Object of \code{\link{R6Class}}.
#' @format An \code{\link{R6Class}} object.
#'
#' @field interval Stores a time interval.
#'
#' @keywords date time
#' @docType class
#'
#' @importFrom R6 R6Class
Period <- R6Class(
  classname = "Period",

  private = list(
    .interval = NA_integer_,
    .negative = FALSE,

    .take_period = function(x, type) {
      assertthat::assert_that(length(x) == 1)

      max_value <- switch(type,
        quarter = 4, third = 3, half = 2
      )

      if (is.null(max_value)) max_value <- 10
      if (x == "last") x <- max_value

      assertthat::assert_that(x %in% 1:max_value,
        msg = "`x` has an invalid value.")

      n_years <- diff(private$.interval) + 1
      step <- floor(n_years / max_value)

      interval <- c(
        (x - 1) * step + private$.interval[1],
        x * step + private$.interval[1] - 1
      )

      if (x == max_value && step %% 3 == 0) {
        interval[2] <- interval[2] + 1
      }

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
    take = function(x = NULL, type = NA) {
      assertthat::assert_that(length(type) == 1)
      type <- tolower(as.character(type))

      interval <- switch(type,
        early = private$.take_early(),
        late = private$.take_late(),
        mid = private$.take_mid(),

        private$.take_period(x, type)
      )

      negative <- ifelse(private$.negative, -1, 1)

      return(sort(interval * negative))
    }
  )
)
