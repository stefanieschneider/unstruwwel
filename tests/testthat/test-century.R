test_that("invalid century", {
  expect_error(Century$new(22))
  expect_error(Century$new("2"))
  expect_error(Century$new(2200))
  expect_error(Century$new(20.22))
  expect_error(Century$new(c(100, 200)))
})

test_that("positive century", {
  expect_equal(Century$new(15)$interval, c(1401, 1500))
  expect_equal(Century$new(1920)$interval, c(1901, 2000))
})

test_that("negative century", {
  expect_equal(Century$new(-15)$interval, c(-1500, -1401))
})

test_that("invalid take", {
  x <- Century$new(15)

  expect_error(x$take(999))
  expect_error(x$take(type = "abc"))
  expect_error(x$take(3, type = "half"))
  expect_error(x$take(4, type = "third"))
  expect_error(x$take(5, type = "quarter"))
})

test_that("take period", {
  x <- Century$new(15)

  expect_equal(x$take(1, type = "half"), c(1401, 1450))
  expect_equal(x$take(2, type = "quarter"), c(1426, 1450))
  expect_equal(x$take("last", type = "third"), c(1467, 1500))

  expect_equal(x$take(type = "early"), c(1401, 1415))
  expect_equal(x$take(type = "late"), c(1486, 1500))
  expect_equal(x$take(type = "mid"), c(1446, 1455))

  expect_equal(x$take(3), c(1421, 1430))
})
