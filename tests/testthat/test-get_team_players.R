
test_that("Nashville Predators 2017-2018", {
  players <- get_team_players("https://www.eliteprospects.com/team/65/nashville-predators/2017-2018")

  comp <- load_test_rds("nashville_predators_2017_players.rds")

  expect_equal(players, comp)
})


test_that("Omaha Lancers 2009-2010", {
  players <- get_team_players("https://www.eliteprospects.com/team/954/omaha-lancers/2009-2010")

  expect_setequal(unique(players$position), c("G", "D", "F"))

  expect_false(any(is.na(players$position)))
})


test_that("Nottingham Panthers 2015-2016", {
  players <- get_team_players("https://www.eliteprospects.com/team/209/nottingham-panthers/2015-2016")

  comp <- load_test_rds("nottingham_panthers_2015_players.rds")

  expect_equal(players, comp)
})
