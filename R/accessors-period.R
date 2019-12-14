#' Set a Period and Get its Time Interval
#'
#' An Object of \code{\link{R6Class}} with methods to set
#' common time periods and specifications for time periods.
#'
#' @keywords date time
#' @docType class
#'
#' @importFrom R6 R6Class
Period <- R6Class(
  classname = "Period",

  private = list(
    #' @field .interval Stores a time interval.
    .interval = NA_integer_,

    #' @field .negative If `TRUE`, a time period is negative.
    .negative = FALSE,

    #' @description
    #' Helper function to specify a time period.
    #'
    #' @param x A numerical scalar. The range of valid values
    #' depends on \code{type}. If \code{type} is \code{"early"},
    #' \code{"mid"}, or \code{"late"}, \code{x} is ignored.
    #' @param type A character scalar. The following values
    #' are supported: \code{"early"}, \code{"mid"}, \code{"late"},
    #' \code{"quarter"}, \code{"third"}, and \code{"half"}. If
    #' \code{type} is `NULL`, \code{x} defines a year or decade.
    #' @param ignore_errors If `TRUE`, error messages are ignored.
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
    #' @field interval Convert and return a time interval.
    interval = function(value) {
      if (missing(value)) {
        negative <- ifelse(private$.negative, -1, 1)
        x <- sort(private$.interval * negative)

        if (self$express < 0) x[1] <- -Inf
        if (self$express > 0) x[2] <- Inf

        return(x)
      } else {
        stop("`$interval` is read only.", FALSE)
      }
    },

    #' @field text Convert and return a text in ISO 8601.
    text = function(value) {
      if (missing(value)) {
        x <- unique(as.character(self$interval))

        if (self$fuzzy < 0) x <- paste0(x, "~", collapse = NULL)
        if (self$fuzzy > 0) x <- paste0(x, "?", collapse = NULL)

        if (self$express < 0) x <- paste0("..", x[2], collapse = "")
        if (self$express > 0) x <- paste0(x[1], "..", collapse = "")

        return(paste(x, collapse = "/"))
      } else {
        stop("`$text` is read only.", FALSE)
      }
    }
  ),

  public = list(
    #' @field fuzzy Either `-1` (approximate) or `1` (uncertain).
    fuzzy = 0,

    #' @field express Either `-1` (before) or `1` (after).
    express = 0,

    #' @description
    #' Create a time period.
    #'
    #' @param ... Numerical scalars or objects of class \code{Period}.
    initialize = function(...) {
      withCallingHandlers({
        x <- unlist(list(...), recursive = TRUE)

        fuzzy <- lapply(x, function(x) {
          if ("Period" %in% class(x)) x$fuzzy
        })

        express <- lapply(x, function(x) {
          if ("Period" %in% class(x)) x$express
        })

        x <- lapply(x, function(x) {
          if ("Period" %in% class(x)) x$interval else x
        })

        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        x <- c(na.omit(as.integer(x)), recursive = TRUE)

        assertthat::assert_that(length(x) > 0)

        if (max(x) < 0) private$.negative <- TRUE
        private$.interval <- c(min(x), c(max(x)))

        if (length(unlist(fuzzy)) > 0) {
          if (any(unlist(fuzzy) < 0)) self$fuzzy <- -1
          if (any(unlist(fuzzy) > 0)) self$fuzzy <- 1
        }

        if (length(unlist(express)) > 0) {
          if (any(unlist(express) < 0)) self$express <- -1
          if (any(unlist(express) > 0)) self$express <- 1
        }
      }, warning = function(event) {
        invokeRestart("muffleWarning")
      })
    },

    #' @description
    #' Set additions for a time period.
    #'
    #' @param x A character vector.
    set_additions = function(x) {
      if (any(c("approximate", "?") %in% x)) self$fuzzy <- -1
      if (any(c("uncertain") %in% x)) self$fuzzy <- 1

      if ("before" %in% x) self$express <- -1
      if ("after" %in% x) self$express <- 1

      return(self)
    },

    #' @description
    #' Specify a period.
    #'
    #' @param x A numerical scalar. The range of valid values
    #' depends on \code{type}. If \code{type} is \code{"early"},
    #' \code{"mid"}, or \code{"late"}, \code{x} is ignored.
    #' @param type A character scalar. The following values
    #' are supported: \code{"early"}, \code{"mid"}, \code{"late"},
    #' \code{"quarter"}, \code{"third"}, and \code{"half"}. If
    #' \code{type} is `NULL`, \code{x} defines a year or decade.
    #' @param ignore_errors If `TRUE`, error messages are ignored.
    #'
    #' @return Object of \code{\link{R6Class}} with methods to set
    #' common time periods and specifications for time periods.
    take = function(x = NA, type = NA, ignore_errors = FALSE) {
      suppressWarnings({
        tryCatch({
          if (length(x) == 2) type <- x[2]; x <- x[1]
          if (!is.na(x) & x != "last") x <- as.numeric(x)

          assertthat::assert_that(length(type) == 1)
          type <- tolower(as.character(type))

          interval <- switch(type,
            early = private$.take_early(),
            late = private$.take_late(),
            mid = private$.take_mid(),

            private$.take_period(x, type)
          )

          negative <- ifelse(private$.negative, -1, 1)

          interval <- sort(interval * negative)
          new_period <- Period$new(interval)

          new_period$fuzzy <- self$fuzzy
          new_period$express <- self$express

          return(new_period)
        }, error = function(event) {
          if (!ignore_errors) stop(event)
          else return(self)
        })
      })
    }
  )
)
