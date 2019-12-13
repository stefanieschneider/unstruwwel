test_that("guess english", {
  x <- filter(get("schemes"), language == "en")$value
  expect_equal(guess_language(x, verbose = FALSE), "en")
})

test_that("guess german", {
  x <- filter(get("schemes"), language == "de")$value
  expect_equal(guess_language(x, verbose = FALSE), "de")
})

test_that("guess french", {
  x <- filter(get("schemes"), language == "fr")$value
  expect_equal(guess_language(x, verbose = FALSE), "fr")
})

test_that("guess multiple", {
  set.seed(20190706) # for reasons of reproducibility

  x <- group_by(get("schemes"), language) %>%
    dplyr::sample_n(100) %>% dplyr::pull(value)

  expect_setequal(
    guess_language(x, verbose = FALSE), c("fr", "en")
  )
})

test_that("guess none", {
  if (interactive()) skip("Session is interactive.")
  expect_error(guess_language(1750:1900, verbose = FALSE))
})

test_that("show message", {
  x <- filter(get("schemes"), language == "de")$value

  expect_success(
    expect_message(guess_language(x, verbose = TRUE))
  )
})
