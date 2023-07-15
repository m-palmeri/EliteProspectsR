
test_that("Milwaukee Admirals test", {
  team_info <- get_team_info("https://www.eliteprospects.com/team/100/milwaukee-admirals")

  expect_equal(team_info$team_id, 100)
  expect_equal(team_info$team_name, "Milwaukee Admirals")
  expect_equal(team_info$league_name, "AHL")
})


test_that("Moose Jaw Warriors test", {
  team_info <- get_team_info("https://www.eliteprospects.com/team/672/moose-jaw-warriors")

  expect_equal(team_info$team_id, 672)
  expect_equal(team_info$team_name, "Moose Jaw Warriors")
  expect_equal(team_info$league_name, "WHL")
})


test_that("Union College test", {
  team_info <- get_team_info("https://www.eliteprospects.com/team/1366/union-college")

  expect_equal(team_info$team_id, 1366)
  expect_equal(team_info$team_name, "Union College")
  expect_equal(team_info$league_name, "NCAA")
})


test_that("CSKA Moscow test", {
  team_info <- get_team_info("https://www.eliteprospects.com/team/187/cska-moskva")

  expect_equal(team_info$team_id, 187)
  expect_equal(team_info$team_name, "CSKA Moskva")
  expect_equal(team_info$league_name, "KHL")
})
