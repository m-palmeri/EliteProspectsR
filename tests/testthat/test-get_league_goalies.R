
test_that("error catching", {
  expect_error(get_league_goalies(website = NULL, league = NULL, season = NULL))
  expect_error(get_league_goalies(website = "test", league = "test", season = NULL))
  expect_error(get_league_goalies(league = "NHL", season = NULL))
})


test_that("parameter check", {
  #using 2010 nhl as check
  with_website <- get_league_goalies(website = "https://www.eliteprospects.com/league/nhl/stats/2010-2011")

  with_uppercase <- get_league_goalies(league = "NHL", season = "2010-2011")

  with_lowercase <- get_league_goalies(league = "nhl", season = "2010-2011")

  comp <- load_test_rds("nhl_2010_2011_goalies.rds")

  expect_equal(with_website, comp)
  expect_equal(with_uppercase, comp)
  expect_equal(with_lowercase, comp)

})


test_that("WHL 2015 check", {
  whl_2015 <- get_league_goalies(league = "WHL", season = "2015-2016")

  comp <- load_test_rds("whl_2015_2016_goalies.rds")

  expect_equal(whl_2015, comp)
})


test_that("WHA All-Time check", {
  wha_alltime <- get_league_goalies(league = "WHA", season = "all-time")

  comp <- load_test_rds("wha_alltime_goalies.rds")

  expect_equal(wha_alltime, comp)
})

