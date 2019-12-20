test_that("invalid period", {
  x <- Periods$new(interval(ymd("1750-01-01"), ymd("1750-12-31")))

  expect_error(x$interval <- interval(ymd("1760-01-01"), ymd("1760-12-31")))
  expect_error(x$iso_format <- "1760-01-01?/1760-12-31?")
  expect_error(x$time_span <- c(1760, 1760))
})
