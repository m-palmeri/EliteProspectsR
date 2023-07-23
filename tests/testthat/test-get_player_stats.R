

test_that("deeper test of output", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  stats <- current_pull[[1]]

  # season is all full
  expect_false(any(is.na(stats$season)))

  #type checks
  column_types <- sapply(stats, class) %>%
    unname()
  expect_equal(column_types,
               c("numeric", rep("character", 3), "numeric",
                 rep("character", 4), rep("numeric", 6)))

})


test_that("Connor McDavid Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_rds("connor_mcdavid_data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("Nicholas West Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/289172/nicholas-west")

  comparative <- load_test_rds("nicholas_west_data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
})


test_that("Filip Forsberg Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/29626/filip-forsberg")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(season %in% paste0("20", 11:20, "-", 12:21))

  comparative <- load_test_rds("filip_forsberg_data.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("John Blum Test", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/68038/john-blum")

  comparative <- load_test_rds("john_blum_data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})


test_that("Pekka Rinne Tests", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/4282/pekka-rinne")

  comparative <- load_test_rds("pekka_rinne_data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})

test_that("Tommy Green Tests", {
  current_pull <- get_player_stats("https://www.eliteprospects.com/player/96540/tommy-green")

  comparative <- load_test_rds("tommy_green_data.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})

