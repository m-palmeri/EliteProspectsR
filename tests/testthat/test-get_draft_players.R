

test_that("error checks", {
  # draft name check
  expect_error(get_draft_players(draft_name = "NHL Draft"),
               "'NHL Draft' not a recognized draft name. Use `draft_names_crosswalk` to see a list of draft names.")

  expect_error(get_draft_players(draft_name = "NHL Draft"),
               "'NHL Draft' not a recognized draft name. Use `draft_names_crosswalk` to see a list of draft names.")

  expect_error(get_draft_players(website = NULL,
                                 draft_name = NULL,
                                 draft_year = NULL),
               "Please specify either the full `website`, or the `draft_name` and `draft_year`")

  expect_error(get_draft_players(website = "website test",
                                 draft_year = 2022),
               "Please use either the `website` parameter, or the `draft_name` and `draft_year` parameter\\(s\\), not both")

  expect_error(get_draft_players(draft_year = 2022),
               "Please supply `draft_name` and `draft_year` parameter\\(s\\)")

  expect_error(get_draft_players(draft_name = "NHL Entry Draft"),
               "Please supply `draft_name` and `draft_year` parameter\\(s\\)")
})


test_that("2022 draft check", {
  draft_table <- get_draft_players(draft_name = "NHL Entry Draft", draft_year = 2022)

  expect_equal(nrow(draft_table), 225)
  expect_equal(draft_table$player[2], "Simon Nemec")
  expect_match(draft_table$player_link[2], "simon-nemec$")
  expect_equal(draft_table$player[30], "Brad Lambert")
  expect_match(draft_table$player_link[30], "brad-lambert$")
})

test_that("2021 draft full check", {
  draft_table <- get_draft_players(draft_name = "NHL Entry Draft", draft_year = 2021)

  comp <- load_test_rds("2021_nhl_draft_table.rds")

  expect_equal(draft_table, comp)
})


test_that("2020 NAHL full check", {
  draft_table1 <- get_draft_players(draft_name = "NAHL Entry Draft", draft_year = 2020)
  draft_table2 <- get_draft_players(draft_name = "nahl entry draft", draft_year = 2020)
  draft_table3 <- get_draft_players(draft_name = "nahl-entry-draft", draft_year = 2020)

  comp <- load_test_rds("2020_nahl_draft_table.rds")

  expect_equal(draft_table1, comp)
  expect_equal(draft_table2, comp)
  expect_equal(draft_table3, comp)
})
