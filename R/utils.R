is_language <- function(x) {
  return(x %in% get("languages")$name)
}

#' @importFrom utf8 utf8_normalize
normalize <- function(x) {
  result <- purrr::set_names(x, utf8_normalize(colnames(x))) %>%
    dplyr::mutate_if(is.character, list(~ utf8_normalize(.)))

  return(result)
}
