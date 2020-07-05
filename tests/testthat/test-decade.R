test_that("invalid decade", {
  expect_error(Decade$new(203))
  expect_error(Decade$new(2021))
  expect_error(Decade$new(197.5))
  expect_error(Decade$new(c(197, 198)))
})

test_that("positive decade", {
  interval_x <- interval(ymd("1770-01-01"), ymd("1779-12-31"))

  expect_equal(Decade$new(1770)$interval, interval_x)
  expect_equal(Decade$new(1770)$time_span, c(1770, 1779))

  interval_x <- interval(ymd("1951-01-01"), ymd("1960-12-31"))

  expect_equal(Decade$new(1950, official_def = TRUE)$interval, interval_x)
  expect_equal(Decade$new(1950, official_def = TRUE)$time_span, c(1951, 1960))
})

test_that("positive decade with take", {
  interval_x <- interval(ymd("1970-01-01"), ymd("1974-12-31"))

  expect_equal(Decade$new(1970)$take(c(1, "half"))$interval, interval_x)
  expect_equal(Decade$new(1970)$take(1, type = "half")$interval, interval_x)

  interval_x <- interval(ymd("1970-01-01"), ymd("1971-12-31"))
  expect_equal(Decade$new(1970)$take(type = "early")$interval, interval_x)

  interval_x <- interval(ymd("1978-01-01"), ymd("1979-12-31"))
  expect_equal(Decade$new(1970)$take(type = "late")$interval, interval_x)

  interval_x <- interval(ymd("1974-01-01"), ymd("1975-12-31"))
  expect_equal(Decade$new(1970)$take(type = "mid")$interval, interval_x)

  interval_x <- interval(ymd("1972-01-01"), ymd("1972-12-31"))
  expect_equal(Decade$new(1970)$take(3)$interval, interval_x)
})

test_that("negative decade", {
  interval_x <- interval(ymd("0000-12-31") - years(1779), ymd("0000-01-01") - years(1770))

  expect_equal(Decade$new(-1770)$interval, interval_x)
  expect_equal(Decade$new(-1770)$time_span, c(-1779, -1770))

  interval_x <- interval(ymd("0000-12-31") - years(1960), ymd("0000-01-01") - years(1951))

  expect_equal(Decade$new(-1950, official_def = TRUE)$interval, interval_x)
  expect_equal(Decade$new(-1950, official_def = TRUE)$time_span, c(-1960, -1951))
})

test_that("negative decade with take", {
  interval_x <- interval(ymd("0000-12-31") - years(1974), ymd("0000-01-01") - years(1970))

  expect_equal(Decade$new(-1970)$take(c(1, "half"))$interval, interval_x)
  expect_equal(Decade$new(-1970)$take(1, type = "half")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1971), ymd("0000-01-01") - years(1970))
  expect_equal(Decade$new(-1970)$take(type = "early")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1979), ymd("0000-01-01") - years(1978))
  expect_equal(Decade$new(-1970)$take(type = "late")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1975), ymd("0000-01-01") - years(1974))
  expect_equal(Decade$new(-1970)$take(type = "mid")$interval, interval_x)

  interval_x <- interval(ymd("0000-01-01") - years(1972), ymd("0000-12-31") - years(1972))
  expect_equal(Decade$new(-1970)$take(3)$interval, interval_x)
})

test_that("invalid take with errors", {
  expect_error(Decade$new(1970)$take(99))
  expect_error(Decade$new(1970)$take(type = "abc"))
  expect_error(Decade$new(1970)$take(3, type = "half"))
  expect_error(Decade$new(1970)$take(4, type = "third"))
  expect_error(Decade$new(1970)$take(5, type = "quarter"))
})

test_that("invalid take without errors", {
  interval_x <- interval(ymd("1970-01-01"), ymd("1979-12-31"))

  expect_equal(Decade$new(1970)$take(99, ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Decade$new(1970)$take(3, "half", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Decade$new(1970)$take(4, "third", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Decade$new(1970)$take(5, "quarter", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Decade$new(1970)$take(type = "abc", ignore_errors = TRUE)$interval, interval_x)
})
