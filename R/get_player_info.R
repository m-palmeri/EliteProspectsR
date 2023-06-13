## player information scraper (birth date, position, etc.)
get_player_info <- function(website) {
  page <- rvest::read_html(website)

  messy_info_table <- page %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_elements("div[class^='order']") %>%
    purrr::map_df(., .f = .player_info_cleaner) %>%
    dplyr::left_join(data.frame(attribute = player_info_vector), ., by = "attribute")
  # player_info_vector is internal vector with player info attributes

  clean_info_table <- messy_info_table %>%
    dplyr::mutate(attribute = tolower(gsub(" ", "_", attribute))) %>%
    dplyr::filter(attribute != "highlights") %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "value") %>%
    cbind(.player_name(website, page), .) %>%
    dplyr::mutate(height = height %>%
                    stringr::str_extract(., "[0-9]+ cm") %>%
                    gsub(" cm", "", .),
                  weight = weight %>%
                    stringr::str_extract(., "[0-9]+ lbs") %>%
                    gsub(" lbs", "", .),
                  cap_hit = cap_hit %>%
                    stringr::str_extract(., "^\\$[0-9,]+") %>%
                    gsub("\\$|,", "", .),
                  drafted = gsub("(round)|(#)|(overall by)", "", drafted)) %>%
    tidyr::separate(drafted, sep = "\\s+", extra = "merge", fill = "left",
                    into = paste("draft", c("year", "round", "pick", "team"), sep = "_")) %>%
    dplyr::mutate(dplyr::across(c(age, height, weight, cap_hit, draft_year, draft_round, draft_pick),
                                as.numeric)) %>%
    replace(., . == "-", NA)

  return(clean_info_table)
}

.player_info_cleaner <- function(html_node) {
  html_node %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    trimws() %>%
    magrittr::set_names(c("attribute", "value")) %>%
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
