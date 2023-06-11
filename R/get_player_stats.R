get_player_stats <- function(website) {
  page <- rvest::read_html(website)

  raw_player_table <- page %>%
    rvest::html_elements("div[id='league-stats']") %>%
    rvest::html_elements("table[class*='player-stats']") %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    set_names(gsub("^$", "NA", names(.))) %>%
    cbind(.player_name(website, page), .)

  player_tables <- list(
    dplyr::select(raw_player_table, id:POST) %>%
      dplyr::select(., -(dplyr::last_col(offset=1):dplyr::last_col())),
    dplyr::select(raw_player_table, id:League, POST:dplyr::last_col()) %>%
      dplyr::select(-POST)
  )

  clean_player_table <- purrr::map(player_tables, .player_stats_cleaner)

  return(clean_player_table)
}


.player_stats_cleaner <- function(df) {
  df %>%
    dplyr::rename(season = S,
                  games_played = GP,
                  goals = G,
                  assists = A,
                  points = TP,
                  penalty_minutes = PIM,
                  plus_minus = `+/-`) %>%
    dplyr::rename_with(tolower) %>%
    replace(., . == "-", NA) %>%
    tidyr::separate(team, into = c("team", "captaincy"), sep = " {10,}", fill = "right") %>%
    dplyr::mutate(dplyr::across(games_played:plus_minus, as.numeric),
                  captaincy = gsub("[^a-zA-Z]", "", captaincy),
                  dplyr::across(dplyr::where(is.character), trimws)) %>%
    dplyr::filter(!is.na(games_played))
}
