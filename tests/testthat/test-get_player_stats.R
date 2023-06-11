test_players <- c("https://www.eliteprospects.com/player/183442/connor-mcdavid",
                  "https://www.eliteprospects.com/player/289172/nicholas-west",
                  "https://www.eliteprospects.com/player/29626/filip-forsberg",
                  "https://www.eliteprospects.com/player/68038/john-blum")


test_that("deeper test of output", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  stats <- current_pull[[1]]

  # season is all full
  expect_false(any(is.na(stats$season)))

  #type checks
  column_types <- sapply(stats, class)
  expect_equal(column_types,
               c("numeric", rep("character", 5), rep("numeric", 6)))

})


test_that("Connor McDavid Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_rds("connor-mcdavid-data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("Nicholas West Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/289172/nicholas-west")

  comparative <- load_test_rds("nicholas-west-data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
})


test_that("Filip Forsberg Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/29626/filip-forsberg")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_rds("filip-forsberg-data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("John Blum Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/68038/john-blum")

  comparative <- load_test_rds("john-blum-data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})

