## code to prepare `league_names_crosswalk` dataset goes here

page <- rvest::read_html("https://www.eliteprospects.com/leagues")

league_links <- page %>%
  rvest::html_elements("ul[class]") %>%
  rvest::html_elements("li") %>%
  rvest::html_elements("a[href*='league']") %>%
  rvest::html_attr("href") %>%
  unique() %>%
  paste0("https://www.eliteprospects.com", .)

league_names_crosswalk <- purrr::map_df(league_links, get_league_info)

usethis::use_data(league_names_crosswalk, overwrite = TRUE)
