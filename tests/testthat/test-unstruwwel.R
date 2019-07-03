context("Unstruwwel")

test_that("approximate date", {
  expect_equal(unstruwwel("um 1460"), "")
})

test_that("uncertain date", {
  expect_equal(unstruwwel(""), "")
})

test_that("unspecified date", {
  expect_equal(unstruwwel(""), "")
})

test_that("date with year", {
  expect_equal(unstruwwel("1842"), "")
})

test_that("date with year intervall", {
  expect_equal(unstruwwel(""), "")
})

test_that("date with year and season", {
  expect_equal(unstruwwel(""), "")
})

test_that("date with year and month", {
  expect_equal(unstruwwel(""), "")
})

test_that("date with year, month, and day", {
  expect_equal(unstruwwel(""), "")
})
