
test_that("general testing", {
  NHL <- get_league_info("https://www.eliteprospects.com/league/nhl")
  expect_equal(NHL$full_name, "National Hockey League")
  expect_equal(NHL$abbreviation, "NHL")
  expect_equal(NHL$link_component, "nhl")

  ECHL <- get_league_info("https://www.eliteprospects.com/league/echl")
  expect_equal(ECHL$full_name, "ECHL")
  expect_equal(ECHL$abbreviation, "ECHL")
  expect_equal(ECHL$link_component, "echl")

  EOJHL <- get_league_info("https://www.eliteprospects.com/league/eojhl")
  expect_equal(EOJHL$full_name, "Eastern Ontario Junior Hockey League")
  expect_equal(EOJHL$abbreviation, "EOJHL")
  expect_equal(EOJHL$link_component, "eojhl")

  VHL <- get_league_info("https://www.eliteprospects.com/league/vhl")
  expect_equal(VHL$full_name, "Vysshaya Hokkeinaya Liga")
  expect_equal(VHL$abbreviation, "VHL")
  expect_equal(VHL$link_component, "vhl")

  WC <- get_league_info("https://www.eliteprospects.com/league/wc")
  expect_equal(WC$full_name, "World Championship")
  expect_equal(WC$abbreviation, "WC")
  expect_equal(WC$link_component, "wc")
})
