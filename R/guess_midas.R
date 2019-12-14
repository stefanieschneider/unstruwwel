#' @importFrom stringr str_detect
#' @importFrom magrittr "%>%"
guess_midas <- function(x, midas = FALSE, verbose = TRUE) {
  count_slash <- str_detect(x, "\\/") %>%
    sum(na.rm = TRUE) %>% `/`(length(x))

  # non-ASCII characters would cause warnings
  valid_dashes <- paste0("0x", 2010:2015) %>%
    purrr::map_chr(intToUtf8) %>% c("-") %>%
    paste0(collapse = "|")

  count_dash <- str_detect(x, valid_dashes) %>%
    sum(na.rm = TRUE) %>% `/`(length(x))

  if (count_dash - count_slash < -0.15 && !midas) {
    if (verbose) {
      message(
        "Please check if input vector might have been ",
        "standardized using MIDAS."
      )
    }
  }

  return(midas)
}

convert_midas <- function(x) {
  return(x)
}
