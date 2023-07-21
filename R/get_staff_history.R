get_staff_history <- function(website) {
  page <- rvest::read_html(website)

  staff_id <- stringr::str_split(website, "/")[[1]] %>%
    Filter(.numeric_identifier, .) %>%
    as.numeric()

  table_setup <- page %>%
    rvest::html_elements("div[id='staff-stats']") %>%
    rvest::html_elements("table")

  team_links <- .get_table_links(table_setup, "team")
  league_links <- .get_table_links(table_setup, "league")

  staff_history <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::rename(season = S,
                  role = `Role on Team`) %>%
    dplyr::rename_with(tolower) %>%
    replace(., . == "", NA) %>%
    tidyr::fill(season) %>%
    dplyr::mutate(team_link = team_links,
                  league_link = league_links,
                  staff_id = staff_id) %>%
    dplyr::select(staff_id, season, team, team_link, league, league_link, role, notes)
}
