test_that("no date", {
  expect_equal(get_item(unstruwwel("undatiert", "de"), 1), c(NA, NA))
})

test_that("approximate date", {
  x <- unstruwwel("1460?", "en", scheme = "object")

  expect_equal(get_item(x, 1)$fuzzy, -1)
  expect_equal(get_item(x, 1)$interval, c(1460, 1460))
})

test_that("uncertain date", {
  x <- unstruwwel("circa 1842", "en", scheme = "object")

  expect_equal(get_item(x, 1)$fuzzy, -1)
  expect_equal(get_item(x, 1)$interval, c(1842, 1842))
})

test_that("date with year", {
  expect_equal(get_item(unstruwwel("1842", "en"), 1), c(1842, 1842))
})

test_that("date with multiple years", {
  x <- unstruwwel("(Guss vor 1906) 1897", "de", scheme = "object")

  expect_equal(get_item(x, 1)$interval, c(-Inf, 1906))
  expect_equal(get_item(x, 2)$interval, c(1897, 1897))
})

test_that("date with year interval", {
  expect_equal(get_item(unstruwwel("1752/60", "en"), 1), c(1752, 1760))
})

test_that("date with year and season", {
  expect_equal(get_item(unstruwwel("Autumn 1945", "en"), 1), c(1945, 1945))
  expect_equal(get_item(unstruwwel("vor dem Sommer 1907", "de"), 1), c(-Inf, 1907))
})

test_that("date with year and month", {
  expect_equal(get_item(unstruwwel("May 1901", "en"), 1), c(1901, 1901))
})

test_that("date with year, month, and day", {
  expect_equal(get_item(unstruwwel("January 1, 1856", "en"), 1), c(1856, 1856))
})

test_that("date with decade", {
  expect_equal(get_item(unstruwwel("1840s", "en"), 1), c(1840, 1849))
})

test_that("date with century", {
  expect_equal(get_item(unstruwwel("19. Jahrhundert", "de"), 1), c(1801, 1900))
  expect_equal(get_item(unstruwwel("1. half 5th century", "en"), 1), c(401, 450))
})
