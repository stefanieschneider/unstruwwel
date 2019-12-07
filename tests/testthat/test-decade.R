test_that("invalid decade", {
  expect_error(Decade$new(203))
  expect_error(Decade$new(2020))
  expect_error(Decade$new(197.5))
  expect_error(Decade$new(c(197, 198)))
})

test_that("positive decade", {
  expect_equal(Decade$new("19")$interval, c(191, 200))
  expect_equal(Decade$new(197)$interval, c(1971, 1980))
  expect_equal(Decade$new(1970)$interval, c(1970, 1979))
})

test_that("negative decade", {
  expect_equal(Decade$new(-197)$interval, c(-1980, -1971))
  expect_equal(Decade$new(-1970)$interval, c(-1979, -1970))
})

test_that("invalid take", {
  x <- Decade$new(1970)

  expect_equal(x$take(99)$interval, c(1970, 1979))
  expect_equal(x$take(type = "abc")$interval, c(1970, 1979))
  expect_equal(x$take(3, type = "half")$interval, c(1970, 1979))
  expect_equal(x$take(4, type = "third")$interval, c(1970, 1979))
  expect_equal(x$take(5, type = "quarter")$interval, c(1970, 1979))
})

test_that("take period", {
  x <- Decade$new(1970)

  # thirds and quarters are usually not passed for decades
  expect_equal(x$take(c(1, "half"))$interval, c(1970, 1974))
  expect_equal(x$take(1, type = "half")$interval, c(1970, 1974))

  expect_equal(x$take(type = "early")$interval, c(1970, 1971))
  expect_equal(x$take(type = "late")$interval, c(1978, 1979))
  expect_equal(x$take(type = "mid")$interval, c(1974, 1975))

  expect_equal(x$take(3)$interval, c(1972, 1972))
})
