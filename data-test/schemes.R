require(stringr)
require(dplyr)

get_scheme <- function(file) {
  data <- tibble::enframe(readRDS(file))

  scheme <- filter(data, !is.na(value)) %>%
    mutate(
      scheme = str_replace_all(value, "[0-9]", "9") %>%
        str_replace_all("[\\p{L}]+", "..."),
      language = str_match(file, "([^/]+).rds$") %>%
        purrr::pluck(2, .default = "")
    ) %>%
    group_by(scheme) %>% sample_n(1) %>%
    filter(!str_detect(value, "\\\\")) %>%
    select(value, scheme, language)

  return(ungroup(scheme))
}

valid_files <- "^[a-z]{2}.rds$" # excludes

schemes <- list.files("./data-test", valid_files,
    full.names = TRUE, ignore.case = TRUE) %>%
  purrr::map(get_scheme) %>% bind_rows() %>%
  unstruwwel:::normalize()

usethis::use_data(schemes, overwrite = TRUE)
