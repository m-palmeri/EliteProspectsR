library(magrittr)

### player_info_vector             -----
website <- "https://www.eliteprospects.com/player/183442/connor-mcdavid"

player_info_vector <- rvest::read_html(website) %>%
  rvest::html_elements("div[class='ep-list']") %>%
  rvest::html_elements("div[class^='order']") %>%
  purrr::map_df(., .f = .person_info_cleaner) %$%
  attribute %>%
  c(., "Status")

rm(website)


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
  saves = "SVS",
  shutouts = "SO",
  record = "WLT",
  time_on_ice = "TOI",
  years_played = "Years",
  wins = "W",
  ties = "T",
  losses = "L",
  Saves = "SV",
  overtime_wins = "OTW",
  overtime_losses = "OTL",
  goals_for = "GF",
  Points_per_game = "P/GP"
)
player_stats_vector <- unlist(player_stats_vector)

### player_stats_vector_character -----
player_stats_vector_character <- c("season", "record")

### staff_info_vector             -----
website <- "https://www.eliteprospects.com/staff/4749/joe-sakic"

staff_info_vector <- rvest::read_html(website) %>%
  rvest::html_elements("div[class='ep-card']") %>%
  rvest::html_elements("div[class='ep-list']") %>%
  rvest::html_children() %>%
  purrr::map_df(., .person_info_cleaner) %$%
  attribute

rm(website)

### write data                    ------

usethis::use_data(player_info_vector,
                  player_stats_vector,
                  player_stats_vector_character,
                  staff_info_vector,
                  internal = T,
                  overwrite = T)
