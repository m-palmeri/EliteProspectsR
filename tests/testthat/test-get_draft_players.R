

test_that("error checks", {
  expect_error(get_draft_players(website = NULL, draft_year = NULL),
               "Please supply either `website` or `draft_year`")

  expect_error(get_draft_players(website = "website test", draft_year = 2022),
               "Please use either the `website` or the `draft_year` parameter, not both")
})


test_that("2022 draft check", {
  draft_table <- get_draft_players(draft_year = 2022)

  expect_equal(nrow(draft_table), 225)
  expect_equal(draft_table$player[2], "Simon Nemec")
  expect_match(draft_table$link[30], "brad-lambert$")
})

test_that("2021 draft full check", {
  draft_table <- get_draft_players(draft_year = 2021)

  comp <- load_test_rds("2021_draft_table.rds")

  expect_equal(draft_table, comp)
})

test_that("2020 draft full check", {
  draft_table <- get_draft_players(draft_year = 2020)

  comp <- load_test_rds("2020_draft_table.rds")

  expect_equal(draft_table, comp)
})


test_that("2020 NAHL full check", {
  draft_table <- get_draft_players(website = "https://www.eliteprospects.com/draft/nahl-entry-draft/2020")

  comp <- load_test_rds("2020_nahl_draft_table.rds")

  expect_equal(draft_table, comp)
})
