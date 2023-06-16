library(magrittr)

### player_info_vector             -----
website <- "https://www.eliteprospects.com/player/183442/connor-mcdavid"

page <- rvest::read_html(website)

player_info_vector <- page %>%
  rvest::html_elements("div[class='ep-list']") %>%
  rvest::html_elements("div[class^='order']") %>%
  purrr::map_df(., .f = .player_info_cleaner) %$%
  attribute %>%
  c(., "Status")

rm(website, page)

### player_stats_vector            -----
player_stats_vector <- list(
  season = "S",
  games_played = "GP",
  goals = "G",
  assists = "A",
  points = "TP",
  penalty_minutes = "PIM",
  plus_minus = "+/-",
  points_per_game = "PPG",
  games_dressed = "GD",
  goals_against_average = "GAA",
  save_percentage = "SV%",
  goals_against = "GA",
  saves = "SV",
  shutouts = "SO",
  record = "WLT",
  time_on_ice = "TOI"
)
player_stats_vector <- unlist(player_stats_vector)

### player_stats_vector_character -----
player_stats_vector_character <- c("season", "record")

### write data                    ------

usethis::use_data(player_info_vector, player_stats_vector, player_stats_vector_character, internal = T, overwrite = T)
