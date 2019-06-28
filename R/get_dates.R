get_dates <- function(x) {
  colnames(x) <- c("value", "digits", "letters")

  x <- tibble::as_tibble(x, .name_repair = "unique") %>%
    dplyr::mutate(letters = tolower(letters))

  return(x)
}
