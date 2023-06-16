

test_that("Connor McDavid Test", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/183442/connor-mcdavid")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(league %in% c("OHL", "WJC-18", "WJC-20"))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(league %in% "OHL")

  comparative <- load_test_rds("connor_mcdavid_careertotals.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("Nicholas West Test", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/289172/nicholas-west")

  comparative <- load_test_rds("nicholas_west_careertotals.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
})


test_that("Filip Forsberg Test", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/29626/filip-forsberg")

  reg_season <- current_pull[[1]] %>%
    dplyr::filter(league %in% c("AHL", "WJC-18", "WJC-20"))

  playoffs <- current_pull[[2]] %>%
    dplyr::filter(league %in% c("AHL", "HockeyAllsvenskan", "J18 Allsvenskan (J18 Nationell)"))

  comparative <- load_test_rds("filip_forsberg_careertotals.rds")

  expect_equal(reg_season, comparative[[1]])
  expect_equal(playoffs, comparative[[2]])
})


test_that("John Blum Test", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/68038/john-blum")

  comparative <- load_test_rds("john_blum_careertotals.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})


test_that("Pekka Rinne Tests", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/4282/pekka-rinne")

  comparative <- load_test_rds("pekka_rinne_careertotals.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})

test_that("Tommy Green Tests", {
  current_pull <- get_player_careertotals("https://www.eliteprospects.com/player/96540/tommy-green")

  comparative <- load_test_rds("tommy_green_careertotals.rds")

  expect_equal(current_pull[[1]], comparative[[1]])
  expect_equal(current_pull[[2]], comparative[[2]])
})
