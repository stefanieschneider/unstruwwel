#' @importFrom stringr str_detect
#' @importFrom magrittr "%>%"
guess_midas <- function(x, midas = FALSE) {
  count_slash <- str_detect(x, "\\/") %>%
    sum(na.rm = TRUE) %>% `/`(length(x))

  count_dash <- str_detect(x, "-|–|—|−") %>%
    sum(na.rm = TRUE) %>% `/`(length(x))

  if (count_dash - count_slash < -0.15) {
    if (isFALSE(midas)) {
      text <- paste(
        "Input vector might have been standardized using",
        "MIDAS. Do you want to convert based on MIDAS?"
      )

      input <- menu(c("Yes", "No"), title = text)
      if (input == 1) midas <- TRUE
    }
  }

  return(midas)
}

convert_midas <- function(x) {

}
