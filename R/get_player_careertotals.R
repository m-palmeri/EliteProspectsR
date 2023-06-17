get_player_careertotals <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='total-stats']") %>%
    rvest::html_elements("table[class*='total-player-stats']")
  raw_player_table <- table_setup %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    magrittr::set_names(gsub("^$", "NA", names(.))) %>%
    cbind(.player_name(website, page), .)

  league_links <- .get_table_links(table_setup, "league")

  player_tables <- list(
    dplyr::select(raw_player_table, id:`|`) %>%
      dplyr::select(., -dplyr::last_col()),
    dplyr::select(raw_player_table, id:League, `|`:dplyr::last_col()) %>%
      dplyr::select(-`|`)
  )

  clean_player_table <- purrr::map(player_tables, .player_stats_cleaner) %>%
    magrittr::set_names(c("Regular Season", "Playoffs")) %>%
    purrr::map(., .f = function(df) {
      df %>%
        dplyr::mutate(league_link = league_links) %>%
        dplyr::select(id:league, league_link, tidyselect::everything())
    }) %>%
    Filter(function(x) nrow(x) > 0, .)
}
