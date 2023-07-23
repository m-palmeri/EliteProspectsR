get_award_winners <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='all-players-awards']") %>%
    rvest::html_elements("table")

  player_links <- .get_table_links(table_setup, "player")
  player_ids <- .get_website_id(player_links)

  award_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    `colnames<-`(ifelse(names(.) == "", "test", names(.))) %>%
    dplyr::rename(season = Season,
                  player = `Award winner`) %>%
    dplyr::mutate(player_id = player_ids) %>%
    dplyr::select(season, player_id, player) %>%
    dplyr::mutate(player = gsub("\\(.*\\)$", "", player),
                  player = trimws(player),
                  player_link = player_links,
                  season = dplyr::if_else(season == "", NA, season)) %>%
    tidyr::fill(season)

  return(award_table)
}

