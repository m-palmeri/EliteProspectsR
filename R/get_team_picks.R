get_team_picks <- function(website) {

  page <- rvest::read_html(website)

  team_id <- .get_website_id(website)

  draft_link <- page %>%
    rvest::html_elements("a[href*='draft']") %>%
    rvest::html_attr("href") %>%
    Filter(function(x) grepl("team", x), .)

  draft_picks <- .get_team_picks_helper(draft_link) %>%
    cbind(team_id, .)

  return(draft_picks)
}


.get_team_picks_helper <- function(draft_link) {
  page <- rvest::read_html(draft_link)

  table_setup <- page %>%
    rvest::html_elements("div[id='drafted-players']") %>%
    rvest::html_elements("table")

  player_links <- .get_table_links(table_setup, "player")

  draft_picks <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::rename(round = Rnd,
                  overall_pick = `#`,
                  player = Player) %>%
    dplyr::mutate(year = player,
                  year = dplyr::if_else(year == overall_pick, year, NA),
                  player = gsub("\\(.*\\)", "", player),
                  player = trimws(player),
                  overall_pick = gsub("\\#", "", overall_pick)) %>%
    dplyr::mutate(dplyr::across(c(year, round, overall_pick), as.numeric)) %>%
    tidyr::fill(year) %>%
    dplyr::filter(overall_pick != year) %>%
    dplyr::mutate(player_link = player_links) %>%
    dplyr::select(year, round, overall_pick, player, player_link)

  return(draft_picks)
}
