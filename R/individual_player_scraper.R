## player information scraper (birth date, position, etc.)
player_info_scraper <- function(website) {
  page <- rvest::read_html(website)

  messy_info_table <- page %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_elements("div[class^='order']") %>%
    purrr::map_df(., .f = .player_info_cleaner)

  clean_info_table <- messy_info_table %>%
    dplyr::filter(attribute != "Highlights") %>%
    dplyr::mutate(attribute = tolower(gsub(" ", "_", attribute))) %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "value")
}

.player_info_cleaner <- function(html_node) {
  html_node %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    trimws() %>%
    set_names(c("attribute", "value")) %>%
    t() %>%
    data.frame()
}


.player_name <- function(website, page) {
  player_id <- stringr::str_split(website, "/")[[1]] %>%
    Filter(.numeric_identifier, .) %>%
    as.numeric()
  player_name <- page %>%
    rvest::html_elements("h1[class$='name']") %>%
    rvest::html_text() %>%
    trimws()

  return(data.frame(id = player_id, name = player_name))
}

.numeric_identifier <- function(x) {
  suppressWarnings(as.numeric(x)) %>%
    is.na() %>%
    isFALSE()
}


player_stats_scraper <- function(website) {
  page <- rvest::read_html(website)

  raw_player_table <- page %>%
    rvest::html_elements("div[id='league-stats']") %>%
    rvest::html_elements("table[class*='player-stats']") %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    set_names(gsub("^$", "NA", names(.)))

  player_tables <- list(
    dplyr::select(raw_player_table, S:POST) %>%
      dplyr::select(., -(dplyr::last_col(offset=1):dplyr::last_col())),
    dplyr::select(raw_player_table, S:League, POST:dplyr::last_col()) %>%
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
