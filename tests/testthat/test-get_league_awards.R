
test_that("error catching", {
  expect_error(get_league_awards(website = NULL, league = NULL))
  expect_error(get_league_awards(website = "test", league = "test"))
})


test_that("parameter check", {
  #using 2022 nhl as check
  with_website <- get_league_awards(website = "https://www.eliteprospects.com/league/nhl")

  with_uppercase <- get_league_awards(league = "NHL")

  with_lowercase <- get_league_awards(league = "nhl")

  comp <- load_test_rds("nhl_awards.rds")

  expect_equal(with_website, comp)
  expect_equal(with_uppercase, comp)
  expect_equal(with_lowercase, comp)

})


test_that("shl awards test", {
  shl_awards <- get_league_awards(league = "SHL")

  expect_true("Swedish Goalie of the Year (Honken Trophy)" %in% shl_awards$award_name)
  expect_true("Swedish Defenseman of the Year (Salming Trophy)" %in% shl_awards$award_name)
  expect_true("SHL Most Valuable Player (Guldhjälmen)" %in% shl_awards$award_name)
  expect_true("SHL Most Points" %in% shl_awards$award_name)
})


test_that("mestis awards test", {
  mestis_awards <- get_league_awards(league = "mestis")

  expect_true(any(grepl("Rookie of the Year", mestis_awards$award_name)))
  expect_true(any(grepl("Gentleman of the Year", mestis_awards$award_name)))
  expect_true(any(grepl("Finland2 .* Champion", mestis_awards$award_name)))
  expect_true(any(grepl("Finland2 .* Playoffs MVP", mestis_awards$award_name)))
})
