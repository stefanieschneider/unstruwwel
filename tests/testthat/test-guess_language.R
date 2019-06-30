test_that("guess english", {
  x <- filter(get("schemes"), language == "en") %>% pull(value)
  expect_equal(guess_language(x, verbose = FALSE), "en")
})

test_that("guess german", {
  x <- filter(get("schemes"), language == "de") %>% pull(value)
  expect_equal(guess_language(x, verbose = FALSE), "de")
})

test_that("guess french", {
  x <- filter(get("schemes"), language == "fr") %>% pull(value)
  expect_equal(guess_language(x, verbose = FALSE), "fr")
})

test_that("guess multiple", {
  x <- group_by(get("schemes"), language) %>%
    sample_n(100) %>% pull(value)

  expect_setequal(
    guess_language(x, verbose = FALSE), c("fr", "de", "en")
  )
})

test_that("guess none", {
  if (interactive()) skip("Session is interactive.")
  expect_error(guess_language(1750:1900, verbose = FALSE))
})

test_that("show message", {
  x <- filter(get("schemes"), language == "de") %>% pull(value)

  expect_success(
    expect_message(guess_language(x, verbose = TRUE))
  )
})

