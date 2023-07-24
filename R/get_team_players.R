get_team_players <- function(website) {
  page <- rvest::read_html(website)

  team_id <- .get_website_id(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='roster']") %>%
    rvest::html_elements("table")

  player_links <- .get_table_links(table_setup, "player", skip_td_filter = T)
  player_ids <- .get_website_id(player_links)

  season <- page %>%
    rvest::html_elements("div[id='roster']") %>%
    rvest::html_text() %>%
    stringr::str_split("\n") %>%
    magrittr::extract2(1) %>%
    grep("[0-9]+-[0-9]+ +Roster", ., value = T) %>%
    gsub("Roster", "", .) %>%
    trimws()

  player_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    replace(., . == "", NA) %>%
    `colnames<-`(c("for_filter", names(.)[2:ncol(.)])) %>%
    tidyr::fill(N) %>%
    dplyr::filter(is.na(for_filter)) %>%
    dplyr::rename(number = `#`,
                  position = N,
                  player_name = Player) %>%
    dplyr::mutate(season = season,
                  number = gsub("\\#", "", number),
                  number = as.numeric(number),
                  player_name = gsub("\\(.*\\).*$", "", player_name),
                  player_name = trimws(player_name),
                  position = stringr::str_sub(position, 1, 1),
                  player_link = player_links,
                  player_id = player_ids,
                  team_id = team_id) %>%
    dplyr::select(team_id, season, number, position, player_id, player_name, player_link)

  return(player_table)
}
