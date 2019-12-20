test_that("invalid year", {
  expect_error(Year$new(2020))
  expect_error(Year$new(197.5))
  expect_error(Year$new(c(197, 198)))
})

test_that("positive year", {
  interval_x <- interval(ymd("1750-01-01"), ymd("1750-12-31"))

  expect_equal(Year$new(1750)$interval, interval_x)
  expect_equal(Year$new(1750)$time_span, c(1750, 1750))
})

test_that("positive year with month", {
  interval_x <- interval(ymd("1750-05-01"), ymd("1750-05-31"))

  expect_equal(Year$new(1750)$take(type = "may")$interval, interval_x)
  expect_equal(Year$new(1750)$take(type = "may")$time_span, c(1750, 1750))
})

test_that("positive year with month and day", {
  interval_x <- interval(ymd("1750-04-05"), ymd("1750-04-05"))

  expect_equal(Year$new(1750)$take(5, "april")$interval, interval_x)
  expect_equal(Year$new(1750)$take(5, "april")$time_span, c(1750, 1750))
})

test_that("negative year", {
  interval_x <- interval(ymd("0000-01-01") - years(1750), ymd("0000-12-31") - years(1750))

  expect_equal(Year$new(-1750)$interval, interval_x)
  expect_equal(Year$new(-1750)$time_span, c(-1750, -1750))
})

test_that("negative year with month", {
  interval_x <- interval(ymd("0000-05-01") - years(1750), ymd("0000-05-31") - years(1750))

  expect_equal(Year$new(-1750)$take(type = "may")$interval, interval_x)
  expect_equal(Year$new(-1750)$take(type = "may")$time_span, c(-1750, -1750))
})

test_that("negative year with month and day", {
  interval_x <- interval(ymd("0000-04-05")- years(1750), ymd("0000-04-05") - years(1750))

  expect_equal(Year$new(-1750)$take(5, "april")$interval, interval_x)
  expect_equal(Year$new(-1750)$take(5, "april")$time_span, c(-1750, -1750))
})
