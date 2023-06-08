website <- "https://www.eliteprospects.com/player/183442/connor-mcdavid"

page <- rvest::read_html(website)

player_info_vector <- page %>%
  rvest::html_elements("div[class='ep-list']") %>%
  rvest::html_elements("div[class^='order']") %>%
  purrr::map_df(., .f = .player_info_cleaner) %$%
  attribute

usethis::use_data(player_info_vector, internal = T)
