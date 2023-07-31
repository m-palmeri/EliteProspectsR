
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

  WJC_20 <- get_league_info("https://www.eliteprospects.com/league/wjc-20")
  expect_equal(WJC_20$full_name, "U20 World Junior Championship")
  expect_na(WJC_20$abbreviation)
  expect_equal(WJC_20$link_component, "wjc-20")
})
