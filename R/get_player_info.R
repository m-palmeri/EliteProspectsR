## player information scraper (birth date, position, etc.)
get_player_info <- function(website) {
  page <- rvest::read_html(website)

  messy_info_table <- page %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_elements("div[class^='order']") %>%
    purrr::map_df(., .f = .person_info_cleaner) %>%
    dplyr::left_join(data.frame(attribute = player_info_vector), ., by = "attribute")
  # player_info_vector is internal vector with player info attributes

  clean_info_table <- messy_info_table %>%
    dplyr::mutate(attribute = tolower(gsub(" ", "_", attribute)),
                  value = gsub("\\s+", " ", value),
                  value = trimws(value)) %>%
    dplyr::filter(attribute != "highlights") %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "value") %>%
    dplyr::mutate(player_id = .get_website_id(website),
                  player_name = .get_person_name(page),
                  height = height %>%
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
    replace(., . == "-", NA) %>%
    dplyr::select(player_id, player_name, tidyselect::everything())

  return(clean_info_table)
}
