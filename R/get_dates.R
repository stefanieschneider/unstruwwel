#' @importFrom dplyr select filter arrange
#' @importFrom tibble as_tibble rowid_to_column
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_dates <- function(x, language) {
  colnames(x) <- c("value", "numeral", "word", "sign")

  x <- as_tibble(x, .name_repair = "unique") %>%
    select(-c("value")) %>% rowid_to_column("id") %>%
    tidyr::gather("key", "value", -c("id")) %>%
    filter(!is.na(.data$value)) %>% arrange(id) %>%
    simplify(language = language)

  return(x)
}

#' @importFrom dplyr bind_rows mutate case_when select
#' @importFrom stringr str_replace_all str_detect
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
simplify <- function(x, language) {
  language <- filter(get("languages"), .data$name %in% language)
  replacements <- bind_rows(language$simplifications)

  x <- mutate(
      x, numeral = lag(.data$key == "numeral", default = FALSE),
      value = case_when(
        !numeral ~ str_replace_all(
          .data$value, purrr::set_names(
            replacements$after, replacements$pattern
          )
        ),
        TRUE ~ .data$value
      ),
      key = case_when(
        str_detect(.data$value, "[0-9]+") ~ "numeral",
        !is.na(.data$value) ~ .data$key
      )
    )

  return(select(x, -c("numeral")))
}
