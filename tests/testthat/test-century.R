test_that("invalid century", {
  expect_error(Century$new(22))
  expect_error(Century$new(2200))
  expect_error(Century$new(20.22))
  expect_error(Century$new(c(100, 200)))
})

test_that("positive century", {
  expect_equal(Century$new("1")$interval, c(1, 100))
  expect_equal(Century$new(15)$interval, c(1401, 1500))
  expect_equal(Century$new(1920)$interval, c(1901, 2000))
})

test_that("negative century", {
  expect_equal(Century$new(-15)$interval, c(-1500, -1401))
})

test_that("invalid take", {
  x <- Century$new(15); y <- c(1401, 1500)

  expect_error(x$take(999))
  expect_error(x$take(type = "abc"))
  expect_error(x$take(3, type = "half"))
  expect_error(x$take(4, type = "third"))
  expect_error(x$take(5, type = "quarter"))

  expect_equal(x$take(999, ignore_errors = TRUE)$interval, y)
  expect_equal(x$take(type = "abc", ignore_errors = TRUE)$interval, y)
  expect_equal(x$take(3, type = "half", ignore_errors = TRUE)$interval, y)
  expect_equal(x$take(4, type = "third", ignore_errors = TRUE)$interval, y)
  expect_equal(x$take(5, type = "quarter", ignore_errors = TRUE)$interval, y)
})

test_that("take period", {
  x <- Century$new(15)

  expect_equal(x$take(c(1, "half"))$interval, c(1401, 1450))
  expect_equal(x$take(1, type = "half")$interval, c(1401, 1450))
  expect_equal(x$take(2, type = "quarter")$interval, c(1426, 1450))
  expect_equal(x$take("last", type = "third")$interval, c(1467, 1500))

  expect_equal(x$take(type = "early")$interval, c(1401, 1415))
  expect_equal(x$take(type = "late")$interval, c(1486, 1500))
  expect_equal(x$take(type = "mid")$interval, c(1446, 1455))

  expect_equal(x$take(3)$interval, c(1421, 1430))
})
