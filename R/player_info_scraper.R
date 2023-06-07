## player information scraper (birth date, position, etc.)
player_info_scraper <- function(website) {
  page <- rvest::read_html(website)

  messy_info_table <- page %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_elements("div[class^='order']") %>%
    purrr::map_df(., .f = .player_info_cleaner)

  clean_info_table <- messy_info_table %>%
    dplyr::mutate(attribute = tolower(gsub(" ", "_", attribute))) %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "value") %>%
    cbind(.player_name(website, page), .)
}

.player_info_cleaner <- function(html_node) {
  html_node %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    trimws() %>%
    set_names(c("attribute", "value")) %>%
    t() %>%
    data.frame()
}


.player_name <- function(website, page) {
  player_id <- stringr::str_split(website, "/")[[1]] %>%
    Filter(.numeric_identifier, .) %>%
    as.numeric()
  player_name <- page %>%
    rvest::html_elements("h1[class$='name']") %>%
    rvest::html_text() %>%
    trimws()

  return(data.frame(id = player_id, name = player_name))
}

.numeric_identifier <- function(x) {
  suppressWarnings(as.numeric(x)) %>%
    is.na() %>%
    isFALSE()
}
