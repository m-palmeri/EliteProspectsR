get_team_players <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='roster']") %>%
    rvest::html_elements("table")

  player_links <- .get_table_links(table_setup, "player", skip_td_filter = T)

  player_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    replace(., . == "", NA) %>%
    `colnames<-`(c("for_filter", names(.)[2:ncol(.)])) %>%
    tidyr::fill(N) %>%
    dplyr::filter(is.na(for_filter)) %>%
    dplyr::rename(number = `#`,
                  position = N,
                  player = Player) %>%
    dplyr::mutate(number = gsub("\\#", "", number),
                  number = as.numeric(number),
                  player = gsub("\\(.*\\).*$", "", player),
                  player = trimws(player),
                  position = stringr::str_sub(position, 1, 1),
                  player_link = player_links) %>%
    dplyr::select(number, position, player, player_link)

  return(player_table)
}
