test_that("no language", {
  expect_error(unstruwwel("1460", scheme = "object"))
})

test_that("invalid language", {
  expect_error(unstruwwel("1460", "bo", scheme = "object"))
})

test_that("no date", {
  expect_equal(get_item(unstruwwel("undatiert", "de")), c(NA, NA))
})

test_that("approximate date", {
  x <- unstruwwel("1460?", "en", scheme = "object")

  expect_equal(get_item(x)$fuzzy, -1)
  expect_equal(get_item(x)$time_span, c(1460, 1460))
  expect_equal(get_item(x)$iso_format, "1460-01-01~/1460-12-31~")
})

test_that("uncertain date", {
  x <- unstruwwel("etwa 1842", "de", scheme = "object")

  expect_equal(get_item(x)$fuzzy, 1)
  expect_equal(get_item(x)$time_span, c(1842, 1842))
  expect_equal(get_item(x)$iso_format, "1842-01-01?/1842-12-31?")
})

test_that("date with year", {
  expect_equal(get_item(unstruwwel("1842", "en")), c(1842, 1842))
})

test_that("date with multiple years", {
  x <- unstruwwel("(Guss vor 1906) 1897", "de", scheme = "object")

  expect_equal(get_item(x, 1)$time_span, c(-Inf, 1905))
  expect_equal(get_item(x, 2)$time_span, c(1897, 1897))

  expect_equal(get_item(x, 1)$iso_format, "..1905-12-31")
  expect_equal(get_item(x, 2)$iso_format, "1897-01-01/1897-12-31")
})

test_that("date with year interval", {
  expect_equal(get_item(unstruwwel("1752/60", "en")), c(1752, 1760))
})

test_that("date with year and season", {
  x <- unstruwwel("Autumn 1945", "en", scheme = "object")

  expect_equal(get_item(x)$time_span, c(1945, 1945))
  expect_equal(get_item(x)$iso_format, "1945-09-01/1945-11-30")

  x <- unstruwwel("vor dem Sommer 1907", "de", scheme = "object")

  expect_equal(get_item(x)$time_span, c(-Inf, 1907))
  expect_equal(get_item(x)$iso_format, "..1907-05-31")
})

test_that("date with year and month", {
  x <- unstruwwel("May 1901", "en", scheme = "object")

  expect_equal(get_item(x)$time_span, c(1901, 1901))
  expect_equal(get_item(x)$iso_format, "1901-05-01/1901-05-31")

  x <- unstruwwel("after June 1860", "en", scheme = "object")

  expect_equal(get_item(x)$time_span, c(1860, Inf))
  expect_equal(get_item(x)$iso_format, "1860-07-01..")
})

test_that("date with year, month, and day", {
  x <- unstruwwel("January 1, 1856", "en", scheme = "object")

  expect_equal(get_item(x)$time_span, c(1856, 1856))
  expect_equal(get_item(x)$iso_format, "1856-01-01/1856-01-01")
})

test_that("date with decade", {
  expect_equal(get_item(unstruwwel("1840s", "en")), c(1840, 1849))
  expect_equal(get_item(unstruwwel("1760er Jahre", "de")), c(1760, 1769))
})

test_that("date with century", {
  x <- unstruwwel("circa 1st half 5th century", "en", scheme = "object")

  expect_equal(get_item(x)$time_span, c(401, 450))
  # expect_equal(get_item(x)$iso_format, "0401-01-01~/0450-12-31~")

  expect_equal(get_item(unstruwwel("19. Jh.", "de")), c(1801, 1900))
  expect_equal(get_item(unstruwwel("5. Jh. v. Chr", "de")), c(-500, -401))

  expect_equal(get_item(unstruwwel("last third 17th cent", "en")), c(1667, 1700))
})

test_that("duplicate dates", {
  x <- unstruwwel(rep(c("late 16th century", "ca. 1920"), 10), "en")

  expect_equal(identical(x[1], x[3]), TRUE)
  expect_equal(identical(x[2], x[4]), TRUE)
})

test_that("midas date with negative year", {
  expect_equal(get_item(unstruwwel("2100ante/1550ante", "de")), c(-2100, -1550))
})
