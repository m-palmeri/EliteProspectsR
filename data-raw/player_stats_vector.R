
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

player_stats_vector_character <- c("season", "record")

usethis::use_data(player_stats_vector, internal = T, overwrite = T)
usethis::use_data(player_stats_vector_character, internal = T, overwrite = T)
