

test_that("error catching", {
  expect_error(get_league_teams(website = NULL, league = NULL, season = NULL))
  expect_error(get_league_teams(website = "test", league = "test", season = NULL))
  expect_error(get_league_teams(league = "NHL", season = NULL))
})


test_that("parameter check", {
  #using 2022 nhl as check
  with_website <- get_league_teams(website = "https://www.eliteprospects.com/league/nhl/2022-2023")

  with_uppercase <- get_league_teams(league = "NHL", season = "2022-2023")

  with_lowercase <- get_league_teams(league = "nhl", season = "2022-2023")

  comp <- load_test_rds("nhl_2022_2023_teams.rds")

  expect_equal(with_website, comp)
  expect_equal(with_uppercase, comp)
  expect_equal(with_lowercase, comp)

})


test_that("KHL 2020 check", {
  khl_2020 <- get_league_teams(league = "KHL", season = "2020-2021")

  comp <- load_test_rds("khl_2020_2021_teams.rds")

  expect_equal(khl_2020$team_link, comp$team_link)
})


test_that("NL 1989 check", {
  nl_1989 <- get_league_teams(league = "NL", season = "1989-1990")

  comp <- load_test_rds("nl_1989_1990_teams.rds")

  expect_equal(sort(nl_1989$team), sort(comp$team))
  expect_equal(sort(nl_1989$team_link), sort(comp$team_link))
})
