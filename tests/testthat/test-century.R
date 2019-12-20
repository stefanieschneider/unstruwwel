test_that("invalid century", {
  expect_error(Century$new(22))
  expect_error(Century$new(20.22))
  expect_error(Century$new(c(10, 20)))
})

test_that("positive century", {
  interval_x <- interval(ymd("0001-01-01"), ymd("0100-12-31"))

  expect_equal(Century$new("1")$interval, interval_x)
  expect_equal(Century$new("1")$time_span, c(1, 100))

  interval_x <- interval(ymd("1401-01-01"), ymd("1500-12-31"))

  expect_equal(Century$new(15)$interval, interval_x)
  expect_equal(Century$new(15)$time_span, c(1401, 1500))
})

test_that("positive century with take", {
  interval_x <- interval(ymd("1401-01-01"), ymd("1450-12-31"))

  expect_equal(Century$new(15)$take(c(1, "half"))$interval, interval_x)
  expect_equal(Century$new(15)$take(1, type = "half")$interval, interval_x)

  interval_x <- interval(ymd("1426-01-01"), ymd("1450-12-31"))
  expect_equal(Century$new(15)$take(2, type = "quarter")$interval, interval_x)

  interval_x <- interval(ymd("1467-01-01"), ymd("1500-12-31"))
  expect_equal(Century$new(15)$take("last", type = "third")$interval, interval_x)

  interval_x <- interval(ymd("1401-01-01"), ymd("1415-12-31"))
  expect_equal(Century$new(15)$take(type = "early")$interval, interval_x)

  interval_x <- interval(ymd("1486-01-01"), ymd("1500-12-31"))
  expect_equal(Century$new(15)$take(type = "late")$interval, interval_x)

  interval_x <- interval(ymd("1446-01-01"), ymd("1455-12-31"))
  expect_equal(Century$new(15)$take(type = "mid")$interval, interval_x)

  interval_x <- interval(ymd("1421-01-01"), ymd("1430-12-31"))
  expect_equal(Century$new(15)$take(3)$interval, interval_x)
})

test_that("negative century", {
  interval_x <- interval(ymd("0000-12-31") - years(100), ymd("0000-01-01") - years(1))

  expect_equal(Century$new("-1")$interval, interval_x)
  expect_equal(Century$new("-1")$time_span, c(-100, -1))

  interval_x <- interval(ymd("0000-12-31") - years(1500), ymd("0000-01-01") - years(1401))

  expect_equal(Century$new(-15)$interval, interval_x)
  expect_equal(Century$new(-15)$time_span, c(-1500, -1401))
})

test_that("negative century with take", {
  interval_x <- interval(ymd("0000-12-31") - years(1450), ymd("0000-01-01") - years(1401))

  expect_equal(Century$new(-15)$take(c(1, "half"))$interval, interval_x)
  expect_equal(Century$new(-15)$take(1, type = "half")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1450), ymd("0000-01-01") - years(1426))
  expect_equal(Century$new(-15)$take(2, type = "quarter")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1500), ymd("0000-01-01") - years(1467))
  expect_equal(Century$new(-15)$take("last", type = "third")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1415), ymd("0000-01-01") - years(1401))
  expect_equal(Century$new(-15)$take(type = "early")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1500), ymd("0000-01-01") - years(1486))
  expect_equal(Century$new(-15)$take(type = "late")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1455), ymd("0000-01-01") - years(1446))
  expect_equal(Century$new(-15)$take(type = "mid")$interval, interval_x)

  interval_x <- interval(ymd("0000-12-31") - years(1430), ymd("0000-01-01") - years(1421))
  expect_equal(Century$new(-15)$take(3)$interval, interval_x)
})

test_that("invalid take with errors", {
  expect_error(Century$new(15)$take(999))
  expect_error(Century$new(15)$take(type = "abc"))
  expect_error(Century$new(15)$take(3, type = "half"))
  expect_error(Century$new(15)$take(4, type = "third"))
  expect_error(Century$new(15)$take(5, type = "quarter"))
})

test_that("invalid take without errors", {
  interval_x <- interval(ymd("1401-01-01"), ymd("1500-12-31"))

  expect_equal(Century$new(15)$take(999, ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Century$new(15)$take(3, "half", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Century$new(15)$take(4, "third", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Century$new(15)$take(5, "quarter", ignore_errors = TRUE)$interval, interval_x)
  expect_equal(Century$new(15)$take(type = "abc", ignore_errors = TRUE)$interval, interval_x)
})
