get_player_stats <- function(website) {
  page <- rvest::read_html(website)

  raw_player_table <- page %>%
    rvest::html_elements("div[id='league-stats']") %>%
    rvest::html_elements("table[class*='player-stats']") %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    magrittr::set_names(gsub("^$", "NA", names(.))) %>%
    cbind(.player_name(website, page), .)

  player_tables <- list(
    dplyr::select(raw_player_table, id:POST) %>%
      dplyr::select(., -(dplyr::last_col(offset=1):dplyr::last_col())),
    dplyr::select(raw_player_table, id:League, POST:dplyr::last_col()) %>%
      dplyr::select(-POST)
  )

  clean_player_table <- purrr::map(player_tables, .player_stats_cleaner) %>%
    magrittr::set_names(c("Regular Season", "Playoffs")) %>%
    Filter(function(x) nrow(x) > 0, .)

  return(clean_player_table)
}


.player_stats_cleaner <- function(df) {
  #skaters
  if ("TP" %in% names(df)) {
    return(.skater_stats_cleaner(df))
  }

  #goalies
  if ("SV%" %in% names(df)) {
    return(.goalie_stats_cleaner(df))
  }
}


.skater_stats_cleaner <- function(df) {
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
                  dplyr::across(dplyr::where(is.character), trimws))
}


.goalie_stats_cleaner <- function(df) {
  df %>%
    dplyr::rename(season = S,
                  games_played = GP,
                  goals_against_average = GAA,
                  save_percentage = `SV%`,
                  goals_against = GA,
                  saves = SV,
                  shutouts = SO,
                  record = WLT,
                  time_on_ice = TOI) %>%
    dplyr::select(-GD) %>%
    dplyr::rename_with(tolower) %>%
    replace(., . == "-", NA) %>%
    dplyr::mutate(dplyr::across(games_played:time_on_ice, as.numeric))
}
