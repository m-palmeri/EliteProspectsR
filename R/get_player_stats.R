get_player_stats <- function(website) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements("div[id='league-stats']") %>%
    rvest::html_elements("table[class*='player-stats']")
  raw_player_table <- table_setup %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    magrittr::set_names(gsub("^$", "NA", names(.))) %>%
    cbind(.player_name(website, page), .)

  team_links <- .get_table_links(table_setup, "team") %>%
    gsub("\\?.*", "", .)
  league_links <- .get_table_links(table_setup, "league")

  player_tables <- list(
    dplyr::select(raw_player_table, id:POST) %>%
      dplyr::select(., -(dplyr::last_col(offset=1):dplyr::last_col())),
    dplyr::select(raw_player_table, id:League, POST:dplyr::last_col()) %>%
      dplyr::select(-POST)
  )

  clean_player_table <- purrr::map(player_tables, .player_stats_cleaner) %>%
    magrittr::set_names(c("Regular Season", "Playoffs")) %>%
    purrr::map(., .f = function(df) {
      df %>%
        dplyr::mutate(team_link = team_links,
                      league_link = league_links) %>%
        dplyr::select(id:team, team_link, captaincy, league, league_link, tidyselect::everything())
    }) %>%
    Filter(function(x) nrow(x) > 0, .)

  return(clean_player_table)
}


.player_stats_cleaner <- function(df) {

  rename_temp <- .rename_df_helper(df)
  numeric_cols <- rename_temp[[2]]

  final_df <- rename_temp[[1]] %>%
    dplyr::rename_with(tolower) %>%
    replace(., . == "-", NA)
  if ("team" %in% names(final_df)) {
    final_df <- final_df %>%
      tidyr::separate(team, into = c("team", "captaincy"), sep = " {10,}", fill = "right") %>%
      dplyr::mutate(captaincy = gsub("[^a-zA-Z]", "", captaincy))
  }
  final_df <- final_df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(numeric_cols), ~ gsub("( )|(\\*)", "", .x)),
                  dplyr::across(tidyselect::all_of(numeric_cols), as.numeric),
                  dplyr::across(dplyr::where(is.character), ~ gsub("\\s{10,}", " ", .x)),
                  dplyr::across(dplyr::where(is.character), trimws))

  return(final_df)
}

