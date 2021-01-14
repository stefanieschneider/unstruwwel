test_that("guess false", {
  x <- filter(get("schemes"), language == "de")$value
  expect_false(guess_midas(x, verbose = FALSE))
})

test_that("show message", {
  expect_success(
    expect_message(guess_language(get("midas"), verbose = TRUE))
  )

  expect_success(
    expect_message(guess_midas(get("midas"), verbose = TRUE))
  )
})
