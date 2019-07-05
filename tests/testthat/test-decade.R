test_that("invalid decade", {
  expect_error(Decade$new("1"))
  expect_error(Decade$new(203))
  expect_error(Decade$new(2020))
  expect_error(Decade$new(197.5))
  expect_error(Decade$new(c(197, 198)))
})

test_that("positive decade", {
  expect_equal(Decade$new(197)$interval, c(1971, 1980))
  expect_equal(Decade$new(1970)$interval, c(1970, 1979))
})

test_that("negative decade", {
  expect_equal(Decade$new(-197)$interval, c(-1980, -1971))
  expect_equal(Decade$new(-1970)$interval, c(-1979, -1970))
})

test_that("invalid take", {
  x <- Decade$new(1970)

  expect_error(x$take(99))
  expect_error(x$take(type = "abc"))
  expect_error(x$take(3, type = "half"))
  expect_error(x$take(4, type = "third"))
  expect_error(x$take(5, type = "quarter"))
})

test_that("take period", {
  x <- Decade$new(1970)

  # thirds and quarters are usually not passed for decades
  expect_equal(x$take(1, type = "half"), c(1970, 1974))

  expect_equal(x$take(type = "early"), c(1970, 1971))
  expect_equal(x$take(type = "late"), c(1978, 1979))
  expect_equal(x$take(type = "mid"), c(1974, 1975))

  expect_equal(x$take(3), c(1972, 1972))
})
