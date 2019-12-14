test_that("positive year", {
  expect_equal(Year$new(1750)$interval, c(1750, 1750))
})

test_that("negative year", {
  expect_equal(Year$new(-1750)$interval, c(-1750, -1750))
})
