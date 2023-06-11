test_players <- c("https://www.eliteprospects.com/player/183442/connor-mcdavid",
                  "https://www.eliteprospects.com/player/289172/nicholas-west",
                  "https://www.eliteprospects.com/player/29626/filip-forsberg",
                  "https://www.eliteprospects.com/player/68038/john-blum")


test_that("deeper test of output", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")[[1]]
})


test_that("Connor McDavid Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_player_table("connor-mcdavid-data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("Nicholas West Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/289172/nicholas-west")

  comparative <- load_test_player_table("nicholas-west-data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
})


test_that("Filip Forsberg Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/29626/filip-forsberg")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_player_table("filip-forsberg-data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("John Blum Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/68038/john-blum")

  comparative <- load_test_player_table("john-blum-data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})

